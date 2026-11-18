#!/usr/bin/env python3
"""Read-only async snapshot archiver for MLB lineups, Polymarket, and Kalshi.

Collects live/pre-game MLB probable pitchers & confirmed batting lineups from the
MLB Stats API alongside active baseball prediction market prices from Polymarket
Gamma and Kalshi public v2 REST endpoints, storing ZSTD-compressed Apache Parquet
partitions under data/raw/mlb_lineups/ and data/raw/markets/.
"""

from __future__ import annotations

import argparse
import asyncio
from dataclasses import dataclass
from datetime import datetime, timezone
import json
import os
from pathlib import Path
import re
from typing import Any, Sequence

import httpx
import pyarrow as pa
import pyarrow.parquet as pq

BASEBALL_PATTERN = re.compile(
    r"\b(mlb|baseball|world series|alcs|nlcs|alds|nlds|cy young|mvp|rookie of the year|home run|strikeout)\b",
    re.IGNORECASE,
)

MLB_SCHEDULE_URL = "https://statsapi.mlb.com/api/v1/schedule"
POLYMARKET_EVENTS_URL = "https://gamma-api.polymarket.com/events"
KALSHI_MARKETS_URL = "https://api.elections.kalshi.com/trade-api/v2/markets"
KALSHI_EVENTS_URL = "https://api.elections.kalshi.com/trade-api/v2/events"


class MarketIngestError(Exception):
    """Base exception for market and lineup snapshot ingestion."""


class UpstreamFetchError(MarketIngestError):
    """Raised when an upstream public REST endpoint returns a failing response."""


@dataclass(frozen=True, slots=True)
class SnapshotConfig:
    """Configuration for passive market and lineup snapshot archiving."""

    lake_root: Path
    target_date: str
    timeout_seconds: float = 15.0
    max_events: int = 200


def resolve_lake_root(explicit: str | None = None) -> Path:
    """Resolve the canonical mlbr data lake root directory."""
    if explicit:
        return Path(explicit).expanduser().resolve()
    env_path = os.environ.get("MLBR_DATA_PATH")
    if env_path:
        return Path(env_path).expanduser().resolve()
    default = Path("/mnt/storage/Rprojects/mlbr")
    return default if default.exists() else Path.cwd()


async def fetch_json(
    client: httpx.AsyncClient,
    url: str,
    params: dict[str, Any] | None = None,
    max_retries: int = 3,
) -> Any:
    """Perform an async GET request with polite 429/503 backoff and JSON decoding."""
    for attempt in range(max_retries):
        try:
            resp = await client.get(url, params=params)
            if resp.status_code in (429, 503) and attempt + 1 < max_retries:
                await asyncio.sleep(1.5 * (2**attempt))
                continue
            resp.raise_for_status()
            return resp.json()
        except httpx.HTTPError as exc:
            if attempt + 1 < max_retries:
                await asyncio.sleep(1.0 * (2**attempt))
                continue
            raise UpstreamFetchError(f"HTTP error fetching {url}: {exc}") from exc
        except ValueError as exc:
            raise UpstreamFetchError(f"Invalid JSON from {url}: {exc}") from exc
    raise UpstreamFetchError(f"Exhausted retries fetching {url}")


def parse_mlb_lineups(payload: dict[str, Any], snapshot_ts: str) -> list[dict[str, Any]]:
    """Extract normalized game, probable pitcher, and batting order records."""
    rows: list[dict[str, Any]] = []
    for date_block in payload.get("dates", []):
        official_date = date_block.get("date")
        for game in date_block.get("games", []):
            game_pk = game.get("gamePk")
            teams = game.get("teams", {})
            lineups = game.get("lineups", {})
            for side, lineup_key in (("home", "homePlayers"), ("away", "awayPlayers")):
                team_block = teams.get(side, {})
                team_info = team_block.get("team", {})
                prob = team_block.get("probablePitcher", {}) or {}
                players = lineups.get(lineup_key, []) or []
                if not players:
                    rows.append({
                        "snapshot_ts_utc": snapshot_ts,
                        "official_date": official_date,
                        "game_pk": int(game_pk) if game_pk is not None else None,
                        "game_type": game.get("gameType"),
                        "status_state": (game.get("status") or {}).get("abstractGameState"),
                        "side": side,
                        "team_id": team_info.get("id"),
                        "team_name": team_info.get("name"),
                        "probable_pitcher_id": prob.get("id"),
                        "probable_pitcher_name": prob.get("fullName"),
                        "batting_order_slot": None,
                        "batter_id": None,
                        "batter_name": None,
                        "primary_position": None,
                    })
                else:
                    for slot, player in enumerate(players, start=1):
                        pos = (player.get("primaryPosition") or {}).get("abbreviation")
                        rows.append({
                            "snapshot_ts_utc": snapshot_ts,
                            "official_date": official_date,
                            "game_pk": int(game_pk) if game_pk is not None else None,
                            "game_type": game.get("gameType"),
                            "status_state": (game.get("status") or {}).get("abstractGameState"),
                            "side": side,
                            "team_id": team_info.get("id"),
                            "team_name": team_info.get("name"),
                            "probable_pitcher_id": prob.get("id"),
                            "probable_pitcher_name": prob.get("fullName"),
                            "batting_order_slot": slot,
                            "batter_id": player.get("id"),
                            "batter_name": player.get("fullName"),
                            "primary_position": pos,
                        })
    return rows


def parse_polymarket_events(
    events: Sequence[dict[str, Any]],
    snapshot_ts: str,
    baseball_only: bool = True,
) -> list[dict[str, Any]]:
    """Extract normalized Polymarket binary/multi-outcome market snapshots."""
    rows: list[dict[str, Any]] = []
    for ev in events:
        title = str(ev.get("title") or "")
        desc = str(ev.get("description") or "")
        slug = str(ev.get("slug") or "")
        tags = " ".join(str(t.get("label") or "") for t in (ev.get("tags") or []) if isinstance(t, dict))
        searchable = f"{title} {slug} {tags} {desc}"
        if baseball_only and not BASEBALL_PATTERN.search(searchable):
            continue
        for m in ev.get("markets") or []:
            rows.append({
                "venue": "polymarket",
                "snapshot_ts_utc": snapshot_ts,
                "event_id": str(ev.get("id") or ""),
                "event_title": title,
                "market_id": str(m.get("id") or ""),
                "ticker_or_slug": str(m.get("slug") or ""),
                "question": str(m.get("question") or ""),
                "outcomes_json": json.dumps(m.get("outcomes")),
                "outcome_prices_json": json.dumps(m.get("outcomePrices")),
                "yes_bid": None,
                "yes_ask": _to_float(m.get("bestAsk")),
                "last_price": _to_float(m.get("lastTradePrice")),
                "spread": _to_float(m.get("spread")),
                "volume_24h": _to_float(ev.get("volume24hr")),
                "liquidity": _to_float(ev.get("liquidityClob") or ev.get("liquidity")),
                "end_date": m.get("endDateIso") or m.get("endDate"),
                "status": "active" if m.get("active") and not m.get("closed") else "closed",
            })
    return rows


def parse_kalshi_markets(
    payload: dict[str, Any],
    snapshot_ts: str,
    baseball_only: bool = True,
) -> list[dict[str, Any]]:
    """Extract normalized Kalshi public v2 market order-book snapshots."""
    rows: list[dict[str, Any]] = []
    for m in payload.get("markets", []) or []:
        title = str(m.get("title") or "")
        ticker = str(m.get("ticker") or "")
        event_ticker = str(m.get("event_ticker") or "")
        searchable = f"{title} {ticker} {event_ticker}"
        if baseball_only and not BASEBALL_PATTERN.search(searchable):
            continue
        yes_bid = _to_float(m.get("yes_bid_dollars"))
        yes_ask = _to_float(m.get("yes_ask_dollars"))
        spread = (yes_ask - yes_bid) if (yes_bid is not None and yes_ask is not None) else None
        rows.append({
            "venue": "kalshi",
            "snapshot_ts_utc": snapshot_ts,
            "event_id": event_ticker,
            "event_title": title,
            "market_id": ticker,
            "ticker_or_slug": ticker,
            "question": str(m.get("yes_sub_title") or title),
            "outcomes_json": json.dumps(["Yes", "No"]),
            "outcome_prices_json": json.dumps([_to_float(m.get("last_price_dollars"))]),
            "yes_bid": yes_bid,
            "yes_ask": yes_ask,
            "last_price": _to_float(m.get("last_price_dollars")),
            "spread": spread,
            "volume_24h": _to_float(m.get("volume_24h_fp")),
            "liquidity": _to_float(m.get("open_interest_fp")),
            "end_date": m.get("expiration_time") or m.get("close_time"),
            "status": str(m.get("status") or ""),
        })
    return rows


def _to_float(value: Any) -> float | None:
    if value is None or value == "":
        return None
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def write_zstd_parquet(rows: list[dict[str, Any]], dest_path: Path, append: bool = True) -> int:
    """Atomically write or append rows to a ZSTD-compressed Parquet file."""
    if not rows:
        return 0
    dest_path.parent.mkdir(parents=True, exist_ok=True)
    new_table = pa.Table.from_pylist(rows)
    if append and dest_path.exists():
        existing = pq.read_table(dest_path)
        table = pa.concat_tables([existing, new_table], promote_options="permissive")
    else:
        table = new_table
    tmp_path = dest_path.with_suffix(f".tmp.{os.getpid()}.parquet")
    pq.write_table(table, tmp_path, compression="zstd")
    tmp_path.replace(dest_path)
    return len(rows)


async def run_snapshot(config: SnapshotConfig, include_all_markets: bool = False) -> dict[str, int]:
    """Fetch MLB lineups, Polymarket, and Kalshi concurrently and persist ZSTD Parquet."""
    snapshot_ts = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    month_tag = config.target_date[:7]

    async with httpx.AsyncClient(timeout=config.timeout_seconds) as client:
        mlb_task = fetch_json(
            client,
            MLB_SCHEDULE_URL,
            params={"sportId": 1, "date": config.target_date, "hydrate": "probablePitcher,lineups"},
        )
        poly_mlb_task = fetch_json(
            client,
            POLYMARKET_EVENTS_URL,
            params={"tag_slug": "mlb", "active": "true", "closed": "false", "limit": config.max_events},
        )
        poly_base_task = fetch_json(
            client,
            POLYMARKET_EVENTS_URL,
            params={"tag_slug": "baseball", "active": "true", "closed": "false", "limit": config.max_events},
        )
        mlb_raw, poly_mlb_raw, poly_base_raw = await asyncio.gather(
            mlb_task, poly_mlb_task, poly_base_task
        )

        # Polite cursor pagination over Kalshi open events with nested markets
        kalshi_markets: list[dict[str, Any]] = []
        cursor: str | None = None
        for _ in range(5):
            k_params: dict[str, Any] = {"status": "open", "with_nested_markets": "true", "limit": 200}
            if cursor:
                k_params["cursor"] = cursor
            k_page = await fetch_json(client, KALSHI_EVENTS_URL, params=k_params)
            for ev in k_page.get("events") or []:
                ev_title = str(ev.get("title") or "")
                ev_series = str(ev.get("series_ticker") or "")
                for m in ev.get("markets") or []:
                    m_copy = dict(m)
                    m_copy.setdefault("title", ev_title)
                    m_copy.setdefault("event_ticker", ev.get("event_ticker") or ev_series)
                    kalshi_markets.append(m_copy)
            cursor = k_page.get("cursor")
            if not cursor:
                break
            await asyncio.sleep(0.3)

    # Deduplicate Polymarket events across mlb + baseball tags
    seen_ev: set[str] = set()
    poly_combined: list[dict[str, Any]] = []
    for ev in list(poly_mlb_raw or []) + list(poly_base_raw or []):
        ev_id = str(ev.get("id") or "")
        if ev_id and ev_id not in seen_ev:
            seen_ev.add(ev_id)
            poly_combined.append(ev)

    lineup_rows = parse_mlb_lineups(mlb_raw, snapshot_ts)
    poly_rows = parse_polymarket_events(poly_combined, snapshot_ts, baseball_only=False)
    kalshi_rows = parse_kalshi_markets({"markets": kalshi_markets}, snapshot_ts, baseball_only=not include_all_markets)

    lineup_path = config.lake_root / "data" / "raw" / "mlb_lineups" / f"lineups-{config.target_date}.parquet"
    poly_path = config.lake_root / "data" / "raw" / "markets" / f"polymarket-{month_tag}.parquet"
    kalshi_path = config.lake_root / "data" / "raw" / "markets" / f"kalshi-{month_tag}.parquet"

    return {
        "mlb_lineup_rows": write_zstd_parquet(lineup_rows, lineup_path, append=True),
        "polymarket_rows": write_zstd_parquet(poly_rows, poly_path, append=True),
        "kalshi_rows": write_zstd_parquet(kalshi_rows, kalshi_path, append=True),
    }


def main() -> None:
    parser = argparse.ArgumentParser(description="Archive read-only MLB lineup and prediction market snapshots.")
    parser.add_argument("--date", default=datetime.now(timezone.utc).strftime("%Y-%m-%d"), help="Target MLB date (YYYY-MM-DD)")
    parser.add_argument("--lake", default=None, help="Explicit mlbr lake root path")
    parser.add_argument("--all-markets", action="store_true", help="Archive all open markets rather than baseball-filtered only")
    args = parser.parse_args()

    cfg = SnapshotConfig(lake_root=resolve_lake_root(args.lake), target_date=args.date)
    counts = asyncio.run(run_snapshot(cfg, include_all_markets=args.all_markets))
    print(json.dumps(counts, indent=2))


if __name__ == "__main__":
    main()
