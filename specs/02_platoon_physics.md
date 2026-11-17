# Specification 02: 4-Quadrant Platoon Physics & Situational Pitching Effectiveness

- **Experiment ID:** `EXP-002-PLATOON-PHYSICS`
- **Architecture Tier:** Level 1 (Pitch Physics & Quality) → Level 2 (Matchup-Adjusted Player True Talent)
- **Authoritative Script:** `models/02_platoon_splits_analysis.R`
- **Published Study:** `site/analyses/02_platoon_physics.qmd`

---

## 1. Objective & Betting Market Rationale

Surface-level pitching statistics (ERA, FIP, season K%) implicitly average over a pitcher's season-long opponent handedness mix. In modern MLB, managers aggressively stack opposite-handed lineups (e.g., 6–7 LHB against a slider/sweeper-heavy RHP). Public prediction markets (Kalshi, Polymarket) routinely misprice game totals (Over/Under), pitcher strikeout props, and moneylines when a pitcher's arsenal has severe **asymmetric platoon decay**.

This specification formalizes the full-population measurement of pitching physics and effectiveness across all **4 handedness quadrants**:
- `RvR` (RHP vs RHB — Same-Hand)
- `LvL` (LHP vs LHB — Same-Hand)
- `RvL` (RHP vs LHB — Opposite-Hand)
- `LvR` (LHP vs RHB — Opposite-Hand)

---

## 2. Coordinate Invariants & Derived Features (Silver Tier)

Evaluated across all pitches in `v_statcast_features`:
1. **Pitcher-Perspective Horizontal Break (`hb_glove_in`, `haa_glove`)**:
   $$\text{hb\_glove\_in} = \text{hb\_in} \times \begin{cases} -1 & \text{if } \text{p\_throws} = \text{L} \\ +1 & \text{if } \text{p\_throws} = \text{R} \end{cases}$$
   Positive values always represent **glove-side break** (Sliders, Sweepers, Cutters); negative values always represent **arm-side run** (Sinkers, Changeups, Splitters).
2. **Batter-Perspective Horizontal Break (`hb_away_in`, `haa_away`)**:
   $$\text{hb\_away\_in} = \text{hb\_in} \times \begin{cases} -1 & \text{if } \text{stand} = \text{L} \\ +1 & \text{if } \text{stand} = \text{R} \end{cases}$$
   Positive values break **away** from the batter; negative values break **into** the batter.
3. **Arm-Side Release Offset (`release_pos_x_arm`)**:
   $$\text{release\_pos\_x\_arm} = \text{release\_pos\_x} \times \begin{cases} +1 & \text{if } \text{p\_throws} = \text{L} \\ -1 & \text{if } \text{p\_throws} = \text{R} \end{cases}$$

---

## 3. Target Effectiveness Metrics (Beyond Whiff Rate)

| Metric | Definition | Why Needed |
| :--- | :--- | :--- |
| **Whiff%** | `SUM(is_whiff) / SUM(is_swing)` | Captures bat-missing deception on swings. |
| **CSW%** | `SUM(is_csw) / COUNT(*)` | Called Strikes + Whiffs; strongest pitch-level predictor of pitcher K%. |
| **Chase%** | `SUM(is_chase) / SUM(1 - is_in_zone)` | Measures out-of-zone swing inducement. |
| **xwOBA** | `AVG(xwoba)` on Balls in Play | Captures contact quality (essential for Sinkers/Cutters). |
| **RV/100** | `AVG(pitcher_run_exp) * 100` | Net pitcher run value per 100 pitches (positive = pitcher advantage). |
| **GB%** | `SUM(is_gb) / SUM(is_bip)` | Ground-ball suppression profile. |

### Platoon Vulnerability Index (PVI)
For any pitcher with sufficient sample in both split regimes ($N_{\text{same}} \ge 300$, $N_{\text{opp}} \ge 300$):
$$\text{PVI}_{\text{xwOBA}} = \text{xwOBA}_{\text{opp}} - \text{xwOBA}_{\text{same}}$$
$$\text{PVI}_{\text{CSW}} = \text{CSW\%}_{\text{same}} - \text{CSW\%}_{\text{opp}}$$
$$\text{PVI}_{\text{RV100}} = \text{RV/100}_{\text{same}} - \text{RV/100}_{\text{opp}}$$

---

## 4. Temporal Validation Protocol

- **Tracking Era Control**: Partitioned by `tracking_era` (`PITCHf/x` $\le 2014$, `TrackMan` $2015\text{–}2019$, `Hawk-Eye` $2020\text{–}2026$).
- **Discovery / Baseline Split**: `2015–2023` (`TrackMan` + early `Hawk-Eye`).
- **Out-of-Time Validation Split**: `2024–2026` (modern `Hawk-Eye`, standardized Sweeper `ST` classification).
