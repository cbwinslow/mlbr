# Specification 02: 4-Quadrant Platoon Physics & Situational Pitching Effectiveness

- **Experiment ID:** `EXP-002-PLATOON-PHYSICS`
- **Architecture Tier:** Level 1 (Pitch Physics & Quality) → Level 2 (Matchup-Adjusted Player True Talent)
- **Authoritative Script:** `models/02_platoon_splits_analysis.R`
- **Published Study:** `site/analyses/02_platoon_physics.qmd`

---

## 1. Mathematical Objective & Theoretical Grounding

### 1.1 Theoretical & Sabermetric Literature Foundation
- **Platoon Asymmetry (Tango, Lichtman & Dolphin, *The Book: Playing the Percentages in Baseball*):** Historical MLB platoon splits average ~15–25 points of wOBA advantage when hitter and pitcher have opposite handedness. However, aggregate platoon adjustments ignore *pitch-mix heterogeneity*—some pitchers suffer severe opposite-hand collapse while others exhibit reverse splits.
- **Aerodynamic Break & Visual Plane Interaction (Nathan; Healey; Marchi & Albert):** Batters align their swing plane horizontally and vertically relative to expected ball arrival. A glove-side breaking ball (`SL`, `ST`) thrown in a same-hand matchup (`RvR`, `LvL`) sweeps *away* from the barrel (`hb_away_in > 0`), whereas in an opposite-hand matchup (`RvL`, `LvR`) it breaks *into* the batter's swing arc (`hb_away_in < 0`). Arm-side offspeed weapons (`CH`, `FS`) reverse this geometry.
- **Times-Through-the-Order (TTO) Familiarity Penalty (Tango; Lichtman):** Starting pitchers lose ~15–25 points of wOBA effectiveness on the 3rd pass through a lineup even when controlling for fatigue, driven by hitter pitch-recognition calibration and compounded by opposite-hand matchups.

### 1.2 Coordinate Invariants (Silver Tier `v_statcast_features`)
1. **Pitcher-Perspective Horizontal Break (`hb_glove_in`, `haa_glove`):**
   $$\text{hb\_glove\_in} = \text{hb\_in} \times \begin{cases} -1 & \text{if } \text{p\_throws} = \text{L} \\ +1 & \text{if } \text{p\_throws} = \text{R} \end{cases}$$
   Positive = **glove-side break** (`ST`, `SL`, `FC`); negative = **arm-side run** (`SI`, `CH`, `FS`).
2. **Batter-Perspective Horizontal Break (`hb_away_in`, `haa_away`):**
   $$\text{hb\_away\_in} = \text{hb\_in} \times \begin{cases} -1 & \text{if } \text{stand} = \text{L} \\ +1 & \text{if } \text{stand} = \text{R} \end{cases}$$
   Positive = breaks **away** from batter; negative = breaks **into** batter.
3. **Arm-Side Release Offset (`release_pos_x_arm`):**
   $$\text{release\_pos\_x\_arm} = \text{release\_pos\_x} \times \begin{cases} +1 & \text{if } \text{p\_throws} = \text{L} \\ -1 & \text{if } \text{p\_throws} = \text{R} \end{cases}$$

---

## 2. Input Features & Data Lake Queries

All queries read directly from Silver `v_statcast_features` (ZSTD Sorted Parquet) joined to `v_chadwick_register`:

```sql
-- 4-Quadrant Arsenal Extraction
SELECT
  CASE WHEN game_year >= 2024 THEN '2024-2026 (Validation)' ELSE '2015-2023 (Discovery)' END AS split_period,
  matchup, p_throws, stand, is_same_hand, pitch_type,
  COUNT(*) AS pitches,
  AVG(release_speed) AS avg_mph,
  AVG(hb_away_in) AS avg_hb_away_in,
  AVG(ivb_in) AS avg_ivb_in,
  SUM(is_whiff)::DOUBLE / NULLIF(SUM(is_swing), 0) * 100 AS whiff_pct,
  AVG(is_csw) * 100 AS csw_pct,
  SUM(is_chase)::DOUBLE / NULLIF(SUM(1 - is_in_zone), 0) * 100 AS chase_pct,
  AVG(xwoba) AS avg_xwoba,
  SUM(is_gb)::DOUBLE / NULLIF(SUM(is_bip), 0) * 100 AS gb_pct,
  AVG(pitcher_run_exp) * 100 AS rv_per_100
FROM v_statcast_features
WHERE game_year >= 2015 AND matchup IS NOT NULL
GROUP BY 1, 2, 3, 4, 5, 6;

-- Starter-Only TTO Extraction (removes 1-inning reliever selection bias)
WITH starters AS (
  SELECT DISTINCT game_pk, pitcher
  FROM v_statcast_features
  WHERE game_year >= 2020 AND inning = 1
)
SELECT f.n_thruorder_pitcher, f.matchup,
       AVG(f.is_csw) * 100 AS csw_pct,
       AVG(f.xwoba) AS avg_xwoba,
       AVG(f.pitcher_run_exp) * 100 AS rv_per_100
FROM v_statcast_features f
INNER JOIN starters s ON f.game_pk = s.game_pk AND f.pitcher = s.pitcher
WHERE f.game_year >= 2020 AND f.matchup IS NOT NULL
GROUP BY 1, 2;
```

---

## 3. Target Variables & Evaluation Metrics

| Metric | Formula | Evaluation Role |
| :--- | :--- | :--- |
| **Whiff%** | `SUM(is_whiff) / SUM(is_swing)` | Bat-missing deception on swings. |
| **CSW%** | `AVG(is_csw)` | Primary pitch-level strikeout/command target. |
| **Chase%** | `SUM(is_chase) / SUM(1 - is_in_zone)` | Out-of-zone swing inducement. |
| **xwOBA** | `AVG(xwoba)` | Contact-damage target (essential for Sinkers/Cutters). |
| **RV/100** | `AVG(pitcher_run_exp) * 100` | Unified net run-prevention value per 100 pitches. |
| **GB%** | `SUM(is_gb) / SUM(is_bip)` | Ground-ball suppression rate. |

### Pitcher Platoon Vulnerability Index (PVI)
For qualified pitchers ($N_{\text{same}} \ge 500$, $N_{\text{opp}} \ge 500$ in 2024–2026):
$$\text{PVI}_{\text{xwOBA}} = \text{xwOBA}_{\text{opp}} - \text{xwOBA}_{\text{same}}, \qquad \text{PVI}_{\text{CSW}} = \text{CSW\%}_{\text{same}} - \text{CSW\%}_{\text{opp}}, \qquad \text{PVI}_{\text{RV100}} = \text{RV/100}_{\text{same}} - \text{RV/100}_{\text{opp}}$$

### Validation Loss / Stability Metrics
Out-of-time stability across `Pitch Type × Quadrant` cells is evaluated via Pearson correlation ($r$) and cell-level consistency between Discovery (`2015–2023`) and Validation (`2024–2026`) splits.

---

## 4. Validation Strategy

- **Tracking Era Control:** Partitioned by `tracking_era` (`PITCHf/x` $\le 2014$, `TrackMan` $2015\text{–}2019$, `Hawk-Eye` $2020\text{–}2026$).
- **Discovery / Baseline Split:** `2015–2023` (`TrackMan` + early `Hawk-Eye`).
- **Out-of-Time Validation Split:** `2024–2026` (modern `Hawk-Eye`, zero look-ahead leakage).

---

## 5. Baseline Comparison

1. **Handedness-Blind Pitch Baseline vs. 4-Quadrant Stratification:** A handedness-blind pitch-type average predicts identical run value for Sweepers (`ST`) and Sinkers (`SI`) regardless of batter side, masking `ST`'s `~0.46–0.77 RV/100` collapse and `SI`'s `>0.50 RV/100` opposite-hand damage surge.
2. **All-Pitcher TTO vs. Starter-Only TTO Baseline:** Naive TTO aggregation mixes 1-inning relievers into `1st TTO`, creating an artificial `-0.7 mph` velocity drop; isolating `inning = 1` starters reveals the true `~0.2 mph` physical velocity shift vs. `~3.0 pct` CSW% perceptual decay.

---

## 6. Implementation Blueprint

- **Silver Materialization:** [`pkg/mlbr/R/features.R`](../pkg/mlbr/R/features.R), [`sql/01_statcast_features.sql`](../sql/01_statcast_features.sql), [`R/materialize_features.R`](../R/materialize_features.R)
- **Research & Gold Tables:** [`models/02_platoon_splits_analysis.R`](../models/02_platoon_splits_analysis.R) → `data/processed/platoon_quadrant_arsenal.parquet`, `data/processed/pitcher_pvi_2024_2026.parquet`, `data/processed/starter_tto_platoon_2020_2026.parquet`
- **Publication & Registry:** [`site/analyses/02_platoon_physics.qmd`](../site/analyses/02_platoon_physics.qmd) & `mlbr::lake_log_experiment("EXP-002-PLATOON-PHYSICS")`
