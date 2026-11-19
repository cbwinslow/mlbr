#!/usr/bin/env python3
"""Lightweight PostToolUse guardrail for mlbr.

Reads Antigravity hook JSON on stdin and emits advisory systemMessage warnings
on stdout when mlbr invariants need attention. Always fails open (exit 0) so it
never breaks a session on unexpected payloads.
"""
from __future__ import annotations

import json
import sys
from pathlib import Path


def main() -> int:
    try:
        payload = json.load(sys.stdin)
    except Exception:
        print(json.dumps({}))
        return 0

    tool_call = payload.get("toolCall") or {}
    args = tool_call.get("args") or {}
    target_path = args.get("Path") or args.get("TargetFile") or args.get("path") or ""
    if not target_path:
        print(json.dumps({}))
        return 0

    p = Path(target_path)
    warnings: list[str] = []

    if p.name == "registry.json" and "experiments" in p.parts:
        warnings.append(
            "mlbr guardrail: experiments/registry.json changed -- remember to run "
            "`Rscript R/generate_agent_manifest.R` before closeout."
        )

    if p.suffix in {".R", ".py", ".sql"} and p.exists() and ".agents" not in p.parts:
        try:
            text = p.read_text(encoding="utf-8", errors="ignore")
            if "/mnt/storage/Rprojects/mlbr" in text:
                warnings.append(
                    f"mlbr guardrail: hardcoded `/mnt/storage/Rprojects/mlbr` found in {p.name}. "
                    "Use `mlbr::default_lake()` (or dynamic path resolution) per AGENTS.md."
                )
        except OSError:
            pass

    if warnings:
        print(json.dumps({"systemMessage": "\n".join(warnings)}))
    else:
        print(json.dumps({}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
