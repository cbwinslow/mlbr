"""Unit tests for the read-only MLB lineup and prediction market snapshot archiver."""

from pathlib import Path
import sys

import pyarrow.parquet as pq

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "R" / "ingest"))
import archive_market_snapshots as ams


def test_parse_mlb_lineups_extracts_pitcher_and_order() -> None:
    payload = {
        "dates": [
            {
                "date": "2026-07-04",
                "games": [
                    {
                        "gamePk": 777123,
                        "gameType": "R",
                        "status": {"abstractGameState": "Preview"},
                        "teams": {
                            "home": {
                                "team": {"id": 147, "name": "New York Yankees"},
                                "probablePitcher": {"id": 543037, "fullName": "Gerrit Cole"},
                            },
                            "away": {
                                "team": {"id": 111, "name": "Boston Red Sox"},
                                "probablePitcher": {"id": 676979, "fullName": "Garrett Crochet"},
                            },
                        },
                        "lineups": {
                            "homePlayers": [
                                {"id": 592450, "fullName": "Aaron Judge", "primaryPosition": {"abbreviation": "CF"}}
                            ],
                            "awayPlayers": [],
                        },
                    }
                ],
            }
        ]
    }
    rows = ams.parse_mlb_lineups(payload, "2026-07-04T15:00:00Z")
    assert len(rows) == 2
    assert rows[0]["side"] == "home"
    assert rows[0]["probable_pitcher_id"] == 543037
    assert rows[0]["batting_order_slot"] == 1
    assert rows[0]["batter_name"] == "Aaron Judge"
    assert rows[1]["side"] == "away"
    assert rows[1]["batting_order_slot"] is None


def test_parse_polymarket_and_kalshi_filter_and_zstd_roundtrip(tmp_path: Path) -> None:
    poly_events = [
        {
            "id": "ev1",
            "title": "2026 MLB World Series Winner",
            "slug": "mlb-world-series-2026",
            "volume24hr": 12500.5,
            "liquidityClob": 84000.0,
            "markets": [
                {
                    "id": "m1",
                    "slug": "yankees-ws",
                    "question": "Will the Yankees win the World Series?",
                    "outcomes": ["Yes", "No"],
                    "outcomePrices": ["0.14", "0.86"],
                    "bestAsk": 0.15,
                    "lastTradePrice": 0.14,
                    "spread": 0.02,
                    "active": True,
                    "closed": False,
                }
            ],
        },
        {
            "id": "ev2",
            "title": "US CPI Inflation November",
            "slug": "us-cpi-nov",
            "markets": [{"id": "m2", "question": "CPI > 3%?"}],
        },
    ]
    kalshi_payload = {
        "markets": [
            {
                "ticker": "MLB-26-WS-NYY",
                "event_ticker": "MLB-26-WS",
                "title": "MLB Baseball World Series Champion",
                "yes_sub_title": "New York Yankees",
                "yes_bid_dollars": "0.1300",
                "yes_ask_dollars": "0.1500",
                "last_price_dollars": "0.1400",
                "volume_24h_fp": "410.00",
                "open_interest_fp": "2500.00",
                "status": "active",
            },
            {
                "ticker": "ECON-CPI",
                "event_ticker": "ECON",
                "title": "CPI Release",
            },
        ]
    }

    poly_rows = ams.parse_polymarket_events(poly_events, "2026-11-18T00:00:00Z")
    kalshi_rows = ams.parse_kalshi_markets(kalshi_payload, "2026-11-18T00:00:00Z")

    assert len(poly_rows) == 1
    assert poly_rows[0]["market_id"] == "m1"
    assert len(kalshi_rows) == 1
    assert abs((kalshi_rows[0]["spread"] or 0.0) - 0.02) < 1e-9

    out_file = tmp_path / "markets.parquet"
    ams.write_zstd_parquet(poly_rows + kalshi_rows, out_file, append=True)
    ams.write_zstd_parquet(kalshi_rows, out_file, append=True)

    meta = pq.read_metadata(out_file)
    assert meta.num_rows == 3
    assert meta.row_group(0).column(0).compression == "ZSTD"
