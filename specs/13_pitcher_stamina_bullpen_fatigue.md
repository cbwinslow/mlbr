# Specification 13: Pitcher Stamina, In-Game Velocity Decay & Dynamic Bullpen Fatigue Engine

- **Experiment ID:** `EXP-013-PITCHER-STAMINA-BULLPEN-FATIGUE`
- **Architecture Tier:** Level 1D / Level 3C — In-Game Fatigue & Pre-Game Bullpen Availability
- **Status:** `SPEC READY FOR IMPLEMENTATION`
- **Upstream Dependencies:**
  - `v_statcast` (`2018–2025`, pitch-by-pitch velocity, pitch count, `game_date`, `game_pk`, `inning_topbot`)
  - `v_retrosheet_games` (`2018–2025`, final game scores & team IDs)
- **Downstream Consumers:** `EXP-014` Level 4 v2 Markov Game Simulator (daily Effective Bullpen FIP & starter fatigue curves).

---

## 1. Mathematical Objective & Theoretical Grounding

### 1.1 Starter Velocity Decay & Stamina Retention
For each starting pitcher outing (`>= 60` pitches), define baseline 1st-inning fastball (`FF`/`SI`) velocity $v_1$ and pitch-count decay:
$$\Delta v(k) = v(k) - v_1, \qquad k \in \{1\text{–}20,\; 21\text{–}40,\; 41\text{–}60,\; 61\text{–}80,\; 81\text{–}100,\; 101+\}$$
Starter Stamina Retention is measured as $\Delta v_{\text{late}} = \bar{v}_{k \ge 75} - \bar{v}_{k \le 25}$ across all regular-season starts (`2018–2025`).

### 1.2 Zero-Leakage Rolling Bullpen Fatigue
For each club $c$ on calendar date $t$, strictly prior to first pitch (`date < t`), compute trailing reliever workload:
$$P_{c,t}^{(1d)} = \sum_{d = t-1} \text{pitches}_{\text{RP}}, \qquad P_{c,t}^{(3d)} = \sum_{d \in [t-3, t-1]} \text{pitches}_{\text{RP}}$$
Estimate the strictly chronological (`2018–2023`) bullpen run-prevention penalty on late-inning runs allowed (`innings 6–9`) and validate strictly OOS on locked `2024–2025`.

---

## 2. Implementation Deliverables
- Model: `models/13_pitcher_stamina_bullpen_fatigue.R` (`--smoke` / `--full`)
- Gold Parquets: `starter_velocity_decay_curve_2018_2025.parquet`, `team_bullpen_fatigue_daily_2018_2025.parquet`, `pitcher_stamina_leaderboard_2024_2025.parquet`
- Study & Lab: `site/analyses/13_pitcher_stamina_bullpen_fatigue.qmd`, `workbooks/lab13_pitcher_stamina_bullpen_fatigue_workbook.R`, `site/workbooks/lab13_pitcher_stamina_bullpen_fatigue_workbook.qmd`
- Visuals: `models/fat01_*`, `models/fat02_*`, `models/fat03_*` (Dark Cyber 4-asset suites, `< 5.0 MB` `.rds`).
