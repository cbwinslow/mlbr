# Specification 04: Chronological True-Talent Player Projections (Marcel, Delta-Method Aging Curves & Tier 1 Statcast Physics Priors)

- **Experiment ID:** `EXP-004-PLAYER-PROJECTIONS`
- **Architecture Tier:** Level 2 (Player-Level True-Talent Projections & Aging Curves)
- **Status:** `DRAFT -> SPEC READY FOR IMPLEMENTATION`
- **Upstream Dependencies:**
  - Bronze/Silver Views: `v_fangraphs_pitch` (`1901–2025`), `v_fangraphs_bat` (`1901–2025`), `v_lahman_people`, `v_chadwick_register`, `v_statcast_features` (`2008–2026`, `game_type = 'R'`)
  - Tier 1 Gold Assets: `data/processed/pitcher_stuff_plus.parquet` (`2020–2026`, strictly guarded by `eval_split` $\in \{\text{train\_insample}, \text{val\_calibration}, \text{test\_oos}\}$)
  - *Explicit Anti-Leakage Note:* Study 02's pooled `data/processed/pitcher_pvi_2024_2026.parquet` spans 2024–2026 without a `game_year` key and MUST NOT be joined into chronological backtests. Instead, season-specific `PVI_xwoba` and `PVI_csw` are computed directly from `v_statcast_features` (`game_type = 'R'`, `game_year <= T-1`) using Study 02's validated equations.
- **Downstream Consumers:** Level 3 Team Run Creation/Prevention (BaseRuns & Pythagenpat), Level 4 Plate-Appearance Monte Carlo Game Simulator, Level 5 Prediction Market Valuation.

---

## 1. Mathematical Objective & Theoretical Grounding

### 1.1 Domain Problem & Sabermetric Literature
Raw single-season baseball statistics (`ERA`, `FIP`, `HR/9`, `wOBA`, `BABIP`) are noisy mixtures of **true underlying talent**, **finite-sample binomial/contact variance**, **aging trajectories**, **role/playing-time changes**, and **seasonal run-environment drift** (e.g., 2019 high-HR ball vs. 2022–2024 deadened ball). Naive prior-year persistence severely overfits noise on low-to-medium sample players and fails to separate lucky outcomes from sustainable physical skill.

This specification establishes `mlbr`'s foundational Tier 2 projection engine by uniting three pillars of modern sabermetric forecasting literature:
1. **Tom Tango's Marcel the Monkey Forecasting System (2004):** Multi-year exponentially recency-weighted counting totals (`5/4/3` for batters; `3/2/1` for pitchers, with an explicit `5/4/3` vs. `3/2/1` pitcher ablation), era-adjusted via seasonal league averages, and regressed toward role-specific league means using Empirical Bayes reliability denominators (sum-before-divide).
2. **Russell Carleton (2012) & Empirical Bayes Stabilization Theory:** Different skill components stabilize at drastically different exposures (`K%` $\approx$ 60–70 PA/TBF, `BB%` $\approx$ 120–170 PA/TBF, `ISO` $\approx$ 160 AB, `HR/9` and `BABIP` $\approx$ 500–800+ BIP/TBF). Applying component-specific reliability shrinkage $M_r$ strictly dominates a single universal regression constant.
3. **Survivor-Bias-Corrected Delta-Method Aging Curves (Tango, Lichtman, Bradbury 2009, Judge 2020):** Cross-sectional averages by age severely understate late-career decline because only elite veterans survive past age 34. The **Delta Method** computes within-player consecutive-season differences $(r_{i,s} - \mu_{\text{lg},s}) - (r_{i,s-1} - \mu_{\text{lg},s-1})$ weighted by harmonic-mean exposure (inverse-variance optimal), smoothed across ages via weighted splines/quadratics. We also explicitly document Lichtman/Judge forced-exit survivor bias as a methodological caveat and test sensitivity on high-exposure cohorts.
4. **Tier 1 Statcast Physics Priors (Eno Sarris / Max Bay Stuff+, Baseball Savant xwOBA, `mlbr` Studies 01–03):** Study 03 proved `mlbr Stuff+` hits split-half $r = 0.975$ in just 100 regular-season pitches and YoY $r = 0.878$ (vs. `ERA` $r = 0.188$). Blending Marcel+Aging rate forecasts with regressed Tier 1 physical priors (`mlbr Stuff+`, `Location+`, `Pitching+`, `Pitching+Tunnel`, seasonal `PVI`, `SwStr%`, `CSW%`, `GB%` for pitchers; `xwOBA`, `Barrel%`, `HardHit%`, `EV90`, `O-Swing%`, `Z-Contact%`, `SwStr%` for batters) via strictly chronological Ridge shrinkage accelerates true-talent convergence without look-ahead leakage.

### 1.2 Exact Mathematical Formulation

#### A. Baseball Innings Conversion & Sum-Before-Divide Invariant
In `v_fangraphs_pitch`, `IP` is stored in baseball outs notation (`.1` $= 1/3$ inning, `.2` $= 2/3$ inning; verified 314–349 baseball-notation rows/season, 0 decimal rows). Every pitcher calculation MUST convert `IP` to exact decimal innings prior to any aggregation:
$$\text{Outs}_{i,s} = 3 \lfloor \text{IP}_{i,s} \rfloor + \text{round}\big(10 \cdot (\text{IP}_{i,s} - \lfloor \text{IP}_{i,s} \rfloor)\big), \qquad \text{IP}_{\text{true},i,s} = \frac{\text{Outs}_{i,s}}{3}$$

All league averages $\mu_{\text{lg},s,g}$ (by season $s$ and role $g \in \{\text{SP}, \text{RP}, \text{BAT}\}$) are computed strictly via **sum-before-divide**:
$$\mu_{\text{lg},s,g}^{\text{K\%}} = \frac{\sum_{i \in g} \text{SO}_{i,s}}{\sum_{i \in g} \text{TBF}_{i,s}}, \qquad \mu_{\text{lg},s,g}^{\text{HR/9}} = 9 \cdot \frac{\sum_{i \in g} \text{HR}_{i,s}}{\sum_{i \in g} \text{IP}_{\text{true},i,s}}, \qquad \mu_{\text{lg},s}^{\text{wOBA}} = \frac{\sum_{i} \text{wOBA}_{i,s} \cdot \text{PA}_{i,s}}{\sum_{i} \text{PA}_{i,s}}$$

#### B. Exact Baseball Age (July-1 Convention)
Verified `6,703 / 6,703` (`100.00%`) exact agreement between `v_fangraphs_pitch.Age` and Chadwick/Lahman birth records:
$$\text{Age}_{i,s} = s - \text{birth\_year}_i - \mathbb{I}(\text{birth\_month}_i \ge 7)$$
This enables exact projected age $\text{Age}_{i,T}$ for future seasons ($T = 2026, 2027$) for all players in `v_chadwick_register` / `v_lahman_people`.

#### C. Role Classification & 2020 COVID-19 Exposure Handling
- **Pitcher Role ($g_i \in \{\text{SP}, \text{RP}\}$):** Determined strictly from prior seasons $\le T-1$ as weighted starter share $\text{SP\_share}_{i,T} = \frac{\sum_{k=1}^3 w_k \text{GS}_{i,T-k}}{\sum_{k=1}^3 w_k \text{G}_{i,T-k}} \ge 0.50 \implies \text{SP}$, else $\text{RP}$.
- **Batter Population Filter:** Exclude hitting pitchers (`COALESCE(position, '') != 'P'`) and zero-PA rows (`PA > 0`). Verified two-way players (e.g., Shohei Ohtani, `xMLBAMID = 660271`) carry `position = 'DH'` and are fully retained.
- **2020 COVID-19 Season (60 games):**
  - *Rate statistics:* Retain raw observed exposure ($N_{i,2020}$ = actual PA/TBF/IP) in the 5/4/3 or 3/2/1 pooling denominator, because binomial/sampling variance depends on actual sample size, not scheduled games.
  - *Playing-time projections:* Pro-rate 2020 exposure by $162/60 = 2.70\times$ (capped at 700 PA for batters, 900 TBF / 215 IP for SP, 320 TBF / 78 IP for RP) before forecasting $T \in \{2021, 2022\}$ playing time:
    $$\widehat{\text{PA}}_{i,T} = 0.50 \cdot \widetilde{\text{PA}}_{i,T-1} + 0.10 \cdot \widetilde{\text{PA}}_{i,T-2} + 200$$
    $$\widehat{\text{TBF}}_{i,T} = \begin{cases} 0.50 \cdot \widetilde{\text{TBF}}_{i,T-1} + 0.10 \cdot \widetilde{\text{TBF}}_{i,T-2} + 220 & \text{if } g_i = \text{SP} \\ 0.50 \cdot \widetilde{\text{TBF}}_{i,T-1} + 0.10 \cdot \widetilde{\text{TBF}}_{i,T-2} + 100 & \text{if } g_i = \text{RP} \end{cases}$$

#### D. Season-Adjusted Marcel + Component-Specific Empirical Bayes Shrinkage
For target season $T$, using strictly prior seasons $T-1, T-2, T-3$ with recency weights $(w_1, w_2, w_3)$ (`5, 4, 3` for batters; `3, 2, 1` primary for pitchers with `5, 4, 3` ablated):
$$W_{i,T} = \sum_{k=1}^{3} w_k \, N_{i,T-k}, \qquad \hat{r}^{\text{Marcel}}_{i,T} = \mu_{\text{lg},T-1,g} + \frac{\sum_{k=1}^{3} w_k \, N_{i,T-k} \, (r_{i,T-k} - \mu_{\text{lg},T-k,g})}{W_{i,T} + M_{r,g}}$$
where the target league baseline is anchored strictly to $\mu_{\text{lg},T-1,g}$ (ZERO future season leakage) and $M_{r,g}$ is the weighted reliability constant estimated chronologically via method-of-moments / cross-validated MSE on seasons $\le T-1$ (starting from Carleton stabilization priors: e.g., $M_{\text{K\%}} \approx 450$, $M_{\text{BB\%}} \approx 900$, $M_{\text{HR/9}} \approx 900\text{ IP}_{\text{w}}$, $M_{\text{wOBA}} \approx 1200$ weighted PA).

#### E. Chronological Harmonic-Mean-Weighted Delta-Method Aging Curves
For every target year $T$, estimate aging transitions strictly from historical consecutive-season pairs $(s-1, s)$ with $s \in [2002, T-1]$ where player $i$ appeared in both seasons with the same role ($g$) and minimum exposure ($N \ge 50$ PA/TBF):
$$\Delta_{i,a} = (r_{i,s} - \mu_{\text{lg},s,g}) - (r_{i,s-1} - \mu_{\text{lg},s-1,g}), \qquad W_{i,a}^{\text{harm}} = \frac{2 \, N_{i,s-1} \, N_{i,s}}{N_{i,s-1} + N_{i,s}}$$
$$\bar{\delta}_{a,g} = \frac{\sum_{i} W_{i,a}^{\text{harm}} \, \Delta_{i,a}}{\sum_{i} W_{i,a}^{\text{harm}}}$$
Fit a smooth exposure-weighted natural cubic spline / quadratic regression $\hat{\delta}_g(a)$ over $a \in [21, 39]$ (clamped conservatively at age boundaries to prevent small-$N$ tail instability). Compute each player's weighted historical age $\bar{a}_{i,T} = \frac{\sum_{k=1}^3 w_k N_{i,T-k} a_{i,T-k}}{W_{i,T}}$ and integrate the expected aging shift from $\bar{a}_{i,T}$ to target age $a_{i,T}$:
$$\hat{r}^{\text{Marcel+Age}}_{i,T} = \hat{r}^{\text{Marcel}}_{i,T} + \int_{\bar{a}_{i,T}}^{a_{i,T}} \hat{\delta}_g(u)\,du$$

#### F. Tier 1 Statcast Physics-Enhanced True-Talent Projections (`M4`)
Let $\mathbf{z}_{i,T-1}$ be the vector of reliability-regressed Tier 1 physics priors computed strictly from seasons $\le T-1$:
- **Pitcher Physics Priors (`M4-P`):**
  - *Long-Horizon Statcast Physics (`2016–2025` rolling OOS):* Regressed `SwStr%`, `CSW%`, `FBv`, `GB%`, `HardHit%`, `Barrel%`, `xERA`, `SIERA`, and seasonal `PVI_xwoba` / `PVI_csw` (computed from `v_statcast_features`, `game_type = 'R'`, `game_year <= T-1`).
  - *Tier 1 `mlbr Stuff+` Physics (`2024 -> 2025` & `2025 -> 2026` locked OOS):* Regressed `mlbr stuff_plus`, `location_plus`, `pitching_plus`, `pitching_tunnel_plus`, `mean_velo_mph`, `csw_pct`, `whiff_pct`, `chase_pct`, `avg_xwoba`, and seasonal `PVI_xwoba` + `PVI_csw`.
- **Batter Physics Priors (`M4-B`):**
  - Regressed 3-year weighted Statcast contact quality and plate discipline: `xwOBA`, `xwOBA - wOBA` (luck residual), `Barrel_pct`, `HardHit_pct`, `EV90`, `EV`, `O-Swing_pct`, `Z-Contact_pct`, and `SwStr_pct`.

Each target rate $y_{i,T}$ is modeled via **exposure-weighted L2-regularized Ridge regression** (`glmnet`, $\alpha = 0$) over the Marcel+Delta-Age baseline, role indicator, age terms, and reliability-shrunk physics priors:
$$\hat{y}^{\text{M4}}_{i,T} = \beta_0 + \beta_{\text{marcel}} \hat{r}^{\text{Marcel+Age}}_{i,T} + \mathbf{\beta}_{\text{phys}}^{\top} \mathbf{z}_{i,T-1} + \beta_{\text{role}} \mathbb{I}(g_i = \text{RP})$$
with regularization penalty $\lambda$ selected strictly via inner chronological / grouped cross-validation on seasons $\le T-1$.

#### G. Component-Reconstructed FIP Check
Alongside direct `FIP` and `SIERA` projections, construct **component-synthesized FIP** from projected `K%`, `BB%`, `HR/9`, and regressed `HBP/TBF` (mapping `TBF/IP` by role) anchored to prior-year $c\text{FIP}_{T-1}$:
$$\widehat{\text{FIP}}^{\text{comp}}_{i,T} = \frac{13 \cdot \widehat{\text{HR/9}}_{i,T} + 3 \cdot (\widehat{\text{BB/9}}_{i,T} + \widehat{\text{HBP/9}}_{i,T}) - 2 \cdot \widehat{\text{K/9}}_{i,T}}{9} + c\text{FIP}_{T-1}$$
to verify internal consistency between component projections and composite run-prevention forecasts.

---

## 2. Input Features & Data Lake Queries

All queries execute via zero-copy DuckDB views (`mlbr::lake_duckdb()`, `SET memory_limit = '12GB'`) with dynamic path resolution (`mlbr::default_lake()`).

### 2.1 Pitcher Historical & Statcast-Era Extraction (`v_fangraphs_pitch` + `v_chadwick_register`)
```sql
SELECT
  p.Season AS game_year,
  p.xMLBAMID AS pitcher_mlbam,
  p.playerid AS fg_id,
  p.PlayerName AS player_name,
  p.team_name_abb AS team,
  p.Throws AS p_throws,
  p.Age AS age,
  cw.birth_year,
  cw.birth_month,
  p.G, p.GS, p.TBF, p.SO, p.BB, COALESCE(p.HBP, 0) AS HBP, p.HR, p.ER,
  (FLOOR(p.IP) + ROUND((p.IP - FLOOR(p.IP)) * 10.0) / 3.0) AS ip_true,
  p.K_pct, p.BB_pct, p.HR_9, p.FIP, p.xFIP, p.SIERA, p.ERA,
  p.GB_pct, p.FB_pct, p.FBv,
  p."SwStr_pct" AS swstr_pct,
  p."C+SwStr_pct" AS csw_pct,
  p.HardHit_pct AS hard_hit_pct,
  p.Barrel_pct AS barrel_pct,
  p.xERA AS xera,
  p.sp_stuff AS fg_stuff_plus,
  p.sp_location AS fg_location_plus,
  p.sp_pitching AS fg_pitching_plus
FROM v_fangraphs_pitch p
LEFT JOIN (
  SELECT DISTINCT key_mlbam, birth_year, birth_month
  FROM v_chadwick_register
  WHERE key_mlbam IS NOT NULL
) cw ON p.xMLBAMID = cw.key_mlbam
WHERE p.Season BETWEEN 2002 AND 2025
  AND p.TBF > 0
  AND p.xMLBAMID IS NOT NULL;
```

### 2.2 Strictly Season-Specific Pitcher PVI & Regular-Season Statcast Summary (`v_statcast_features`, 2015–2026)
```sql
WITH by_hand AS (
  SELECT
    game_year,
    pitcher AS pitcher_mlbam,
    is_same_hand,
    COUNT(*) AS n_pitches,
    SUM(is_csw)::DOUBLE / NULLIF(COUNT(*), 0) AS csw_rate,
    SUM(CASE WHEN xwoba IS NOT NULL THEN xwoba ELSE 0 END) /
      NULLIF(SUM(CASE WHEN xwoba IS NOT NULL THEN 1 ELSE 0 END), 0) AS xwoba_rate,
    SUM(CASE WHEN xwoba IS NOT NULL THEN 1 ELSE 0 END) AS n_xwoba
  FROM v_statcast_features
  WHERE game_type = 'R'
    AND game_year BETWEEN 2015 AND 2026
    AND is_same_hand IS NOT NULL
  GROUP BY 1, 2, 3
)
SELECT
  game_year,
  pitcher_mlbam,
  SUM(CASE WHEN is_same_hand = 1 THEN n_pitches ELSE 0 END) AS n_same_pitches,
  SUM(CASE WHEN is_same_hand = 0 THEN n_pitches ELSE 0 END) AS n_opp_pitches,
  MAX(CASE WHEN is_same_hand = 0 THEN xwoba_rate END) -
    MAX(CASE WHEN is_same_hand = 1 THEN xwoba_rate END) AS pvi_xwoba_raw,
  MAX(CASE WHEN is_same_hand = 1 THEN csw_rate END) -
    MAX(CASE WHEN is_same_hand = 0 THEN csw_rate END) AS pvi_csw_raw
FROM by_hand
GROUP BY 1, 2;
```
*(Reliability-shrunk in R by $\frac{n_{\text{eff}}}{n_{\text{eff}} + 400}$ where $n_{\text{eff}} = \frac{2 n_{\text{same}} n_{\text{opp}}}{n_{\text{same}} + n_{\text{opp}}}$ so low-split relievers shrink cleanly toward 0 PVI.)*

### 2.3 Tier 1 Gold `mlbr Stuff+` Join (`data/processed/pitcher_stuff_plus.parquet`)
Joined strictly on `(pitcher_mlbam = pitcher, game_year)` with an explicit `eval_split` check:
- `2020–2023`: `train_insample` (Study 03 tree model trained on `2015–2023`; ZERO 2024+ data seen)
- `2024`: `val_calibration` (tree OOS; scaled on 2024; ZERO 2025+ data seen)
- `2025–2026`: `test_oos` (fully locked out-of-sample)

### 2.4 Batter Historical & Statcast-Era Extraction (`v_fangraphs_bat` + `v_chadwick_register`)
```sql
SELECT
  b.Season AS game_year,
  b.xMLBAMID AS batter_mlbam,
  b.playerid AS fg_id,
  b.PlayerName AS player_name,
  b.team_name_abb AS team,
  b.Bats AS bats,
  b.position,
  b.Age AS age,
  cw.birth_year,
  cw.birth_month,
  b.G, b.PA, b.AB, b.H, b.HR, b.BB, b.SO, COALESCE(b.HBP, 0) AS HBP,
  b.wOBA AS woba, b.OBP AS obp, b.SLG AS slg, b.ISO AS iso,
  b.BABIP AS babip, b.K_pct AS k_pct, b.BB_pct AS bb_pct,
  b.xwOBA AS xwoba, b.EV AS ev, b.EV90 AS ev90,
  b.Barrel_pct AS barrel_pct, b.HardHit_pct AS hard_hit_pct,
  b."O-Swing_pct" AS o_swing_pct,
  b."Z-Contact_pct" AS z_contact_pct,
  b."SwStr_pct" AS swstr_pct,
  b.wRC_plus AS wrc_plus
FROM v_fangraphs_bat b
LEFT JOIN (
  SELECT DISTINCT key_mlbam, birth_year, birth_month
  FROM v_chadwick_register
  WHERE key_mlbam IS NOT NULL
) cw ON b.xMLBAMID = cw.key_mlbam
WHERE b.Season BETWEEN 2002 AND 2025
  AND b.PA > 0
  AND COALESCE(b.position, '') != 'P'
  AND b.xMLBAMID IS NOT NULL;
```

### 2.5 Regular-Season 2026 Statcast Outcomes Extraction (`v_statcast_features` + `v_statcast`, `game_type = 'R'`)
Used strictly for (a) Secondary Locked OOS validation (`2025 -> 2026` Regular Season, `716,792` pitches, `2,429` games) on `K%`, `BB%` (including `'intent_walk'`), `HR/BF`, and `xwOBA`, and (b) generating live forward-looking `2027` true-talent projections cleanly tagged `eval_split = 'live_2027_forecast'`.

---

## 3. Target Variables & Loss Functions

### 3.1 Pitcher Targets
| Target | Exposure Weight ($w_i$) | Primary Cohort | Secondary Qualified Cohort |
| :--- | :--- | :--- | :--- |
| **`K%`** (`SO / TBF`) | `TBF` | `TBF >= 100` | `TBF >= 300` |
| **`BB%`** (`BB / TBF`) | `TBF` | `TBF >= 100` | `TBF >= 300` |
| **`HR/9`** (`9 * HR / ip_true`) | `ip_true` | `TBF >= 100` | `TBF >= 300` |
| **`FIP`** | `ip_true` | `TBF >= 100` | `TBF >= 300` |
| **`SIERA`** | `TBF` | `TBF >= 100` | `TBF >= 300` |
| **`ERA`** *(diagnostic reference)* | `ip_true` | `TBF >= 100` | `TBF >= 300` |

### 3.2 Batter Targets
| Target | Exposure Weight ($w_i$) | Primary Cohort | Secondary Qualified Cohort |
| :--- | :--- | :--- | :--- |
| **`wOBA`** *(primary)* | `PA` | `PA >= 100` | `PA >= 300` |
| **`K%`** (`SO / PA`) | `PA` | `PA >= 100` | `PA >= 300` |
| **`BB%`** (`BB / PA`) | `PA` | `PA >= 100` | `PA >= 300` |
| **`ISO`** (`SLG - AVG`) | `AB` | `PA >= 100` | `PA >= 300` |
| **`OBP`** & **`SLG`** *(secondary)* | `PA` / `AB` | `PA >= 100` | `PA >= 300` |

### 3.3 Loss Functions & Acceptance Criteria
1. **Exposure-Weighted RMSE (Primary):** $\text{wRMSE} = \sqrt{\frac{\sum_i w_i (\hat{y}_i - y_i)^2}{\sum_i w_i}}$
2. **Exposure-Weighted MAE (Secondary):** $\text{wMAE} = \frac{\sum_i w_i |\hat{y}_i - y_i|}{\sum_i w_i}$
3. **Weighted Pearson $r$ & Calibration Slope/Intercept:** Weighted OLS of observed $y_i$ on projected $\hat{y}_i$ (target slope $\in [0.90, 1.10]$, bias $|\bar{\hat{y}} - \bar{y}|$ minimal).
4. **Statistical Significance:** Exposure-weighted paired MSE difference test (clustered/paired bootstrap $95\%$ CI on $\Delta\text{wRMSE}$ vs. Pure Marcel `B2`).
5. **Acceptance Criteria:**
   - `B2` (Pure Marcel) strictly beats `B0` (League Avg) and `B1` (Naive Prior-Year Persistence) on every metric across `2016–2025` rolling OOS.
   - `M4` (Tier 1 Physics-Enhanced) strictly beats `B2` (Pure Marcel) on locked `2024 -> 2025` OOS `wRMSE` for primary pitcher metrics (`K%`, `FIP`, `SIERA`) and primary batter metrics (`wOBA`, `K%`, `ISO`).
   - Zero temporal leakage verified across every split, aging curve, league anchor, and feature table.

---

## 4. Validation Strategy (Strict Chronological Zero-Leakage Protocol)

| Validation Track | Target Season(s) $T$ | Training / Estimation Window | Features Permitted | Leakage Guard |
| :--- | :--- | :--- | :--- | :--- |
| **Track 1: Historical Rolling Marcel & Aging Backtest** | `2016–2025` (10 OOS seasons) | Aging curves & league anchors estimated strictly on `2002 .. T-1` | `v_fangraphs_pitch` / `v_fangraphs_bat` $\le T-1$ | Target season $T$ never enters aging curves, league mean anchor, or regression tuning |
| **Track 2: Statcast-Era Rolling Physics Backtest (Batters & Pitchers)** | `2019–2025` (7 OOS seasons) | Expanding Ridge fit strictly on target years `2016 .. T-1` | Non-ML Statcast/FG physics (`xwOBA`, `Barrel%`, `HardHit%`, `O-Swing%`, `Z-Contact%`, `SwStr%`, `CSW%`, `FBv`, `GB%`, seasonal `PVI`) $\le T-1$ | Strictly chronological expanding window; zero future years |
| **Track 3: Locked `2024 -> 2025` Tier 1 `mlbr Stuff+` Benchmark (Primary Pitcher Test)** | **`2025`** (FanGraphs locked OOS) | Calibration fold: **`2023 -> 2024`** (`2024` is 100% unseen by Study 03 tree training `2015–2023`) | `2024` `val_calibration` `mlbr Stuff+/Location+/Pitching+` + `2024` seasonal `PVI` + `2022–2024` Marcel | `2025` is untouched `test_oos`; `2020–2023` `train_insample` pairs NEVER evaluated as OOS |
| **Track 4: Secondary Full-OOS `2025 -> 2026` Statcast Verification** | **`2026`** (Statcast Reg-Season, 2,429 games) | Frozen weights from Track 3 (plus sensitivity refit on `2024 -> 2025` where BOTH calibration & test use strictly OOS `Stuff+`) | `2025` `test_oos` `mlbr Stuff+/Location+/Pitching+` + `2025` seasonal `PVI` + `2023–2025` Marcel | Verifies zero weight-calibration distortion from `2023` `train_insample` `Stuff+` |

---

## 5. Baseline Comparison Ladder

Every model tier is evaluated on the exact same matched player cohorts and weighted loss functions:
1. **`B0` (Prior-Year Role League Average):** Predict $\mu_{\text{lg},T-1,g}$ for every player.
2. **`B1` (Naive Prior-Year Persistence):** Predict observed $r_{i,T-1}$ directly (with league-mean fallback only when $N_{i,T-1} = 0$). Quantifies raw regression-to-the-mean penalty.
3. **`B2` (Pure Tango Marcel + Classic Linear Age):** Era-adjusted 3-year weighted sum-before-divide (`5/4/3` batters; `3/2/1` pitchers + `5/4/3` pitcher ablation) + empirical Bayes reliability regression + Tango's classic piecewise-linear age adjustment (`+0.006*(29 - age)` if `< 29`, `-0.003*(age - 29)` if `> 29` scaled to each metric's direction).
4. **`B3` (Marcel + Survivor-Bias-Corrected Empirical Delta-Method Aging):** Replaces fixed linear age slope with harmonic-mean-weighted Delta-Method aging curves estimated strictly on `2002 .. T-1` separately for Batters, Starting Pitchers, and Relief Pitchers.
5. **`M4` (Tier 1 Physics-Enhanced True-Talent Projections — Production `mlbr`):** Blends `B3` with regressed Tier 1 Statcast Physics Priors (`mlbr Stuff+/Location+/Pitching+`, seasonal `PVI`, `SwStr%/CSW%/GB%` for pitchers; `xwOBA`, `Barrel%`, `HardHit%`, `EV90`, `O-Swing%`, `Z-Contact%`, `SwStr%` for batters) via strictly chronological Ridge shrinkage. Also reports an external reference comparison against FanGraphs `sp_pitching` (`Pitching+`) on `2024 -> 2025`.

---

## 6. Implementation Blueprint

### 6.1 Authoritative Script & Gold Parquet Deliverables
1. **Authoritative Modeling & Visualization Script:** `models/04_player_projections_marcel_aging.R`
   - Resolves lake root dynamically via `mlbr::default_lake()`, configures DuckDB `memory_limit = '12GB'`, and enforces fail-fast schema/null/join assertions (`stopifnot` / explicit `stop()`).
   - Executes Tracks 1–4 chronologically, runs the `3/2/1` vs `5/4/3` pitcher weight ablation and `2023->2024` vs `2024->2025` calibration sensitivity check.
   - Writes Gold Tier ZSTD-compressed sorted Parquet outputs (`arrow::write_parquet(..., compression = "zstd")`):
     - `data/processed/pitcher_projections_true_talent.parquet` (sorted by `projection_year, pitcher_mlbam`; covers `2016–2025` OOS backtest + `2026` forward + `2027` live projections, tagged with `eval_split` and `model_version`).
     - `data/processed/batter_projections_true_talent.parquet` (sorted by `projection_year, batter_mlbam`; covers `2016–2025` OOS backtest + `2026` forward + `2027` live projections, tagged with `eval_split` and `model_version`).
     - `data/processed/projections_aging_curves.parquet` (Delta-Method vs. naive cross-sectional aging curves by role & metric).
     - `data/processed/projections_backtest_metrics.parquet` (complete `B0`–`M4` weighted RMSE, MAE, Pearson $r$, calibration slope, and bootstrap CIs across all tracks).
   - Logs validated formulas, metrics, and artifact paths into `experiments/registry.json` via `mlbr::lake_log_experiment("EXP-004-PLAYER-PROJECTIONS", ...)`.

### 6.2 Dark Cyber 4-Asset Visual Suite (`models/proj01_*` .. `models/proj05_*`)
Every plot uses `mlbr::theme_cyber()`, `mlbr::save_plot_assets()` (ZSTD Parquet + 300 DPI `.png` + lean `< 5 MB` closure-stripped `.rds` + standalone interactive `.html` for Plotly widgets):
1. **`proj01_empirical_aging_curves`**: Multi-panel survivor-bias-corrected Delta-Method aging curves (Ages 21–39) vs. biased cross-sectional curves for Batters (`wOBA`, `ISO`, `K%`, `BB%`) and Pitchers (`SP` vs `RP`: `K%`, `BB%`, `FIP`, `FBv`).
2. **`proj02_stabilization_reliability_shrinkage`**: Empirical Bayes reliability weight curves across exposure (`50–700` PA/TBF) comparing lagging outcome stats (`ERA`, `BABIP`, `HR/9`, `wOBA`) against fast-stabilizing skills (`K%`, `BB%`) and Tier 1 Physics (`mlbr Stuff+`, `xwOBA`).
3. **`proj03_oos_backtest_ablation`**: Multi-season (`2016–2025`) and locked `2024 -> 2025` exposure-weighted RMSE & MAE improvement across the full baseline ladder (`B0` -> `B1` -> `B2` -> `B3` -> `M4`) for Pitchers and Batters.
4. **`proj04_pitcher_projections_explorer`** *(Static `.png` + Interactive Plotly `.html`)*: Pitcher True-Talent Frontier — Observed `2025` `FIP`/`ERA` vs. `mlbr` Projected `2026` `FIP`/`SIERA`, colored by `mlbr Stuff+` and sized by projected `TBF`, surfacing breakout buys vs. regression traps.
5. **`proj05_batter_projections_explorer`** *(Static `.png` + Interactive Plotly `.html`)*: Batter True-Talent Frontier — Observed `2025` `wOBA` vs. `mlbr` Projected `2026` `wOBA`, colored by Statcast `xwOBA - wOBA` skill residual and sized by projected `PA`.

### 6.3 Quarto Publication, Deterministic Closeout & Independent Audits
1. Publish 7-section Q-CRISP study at `site/analyses/04_player_projections.qmd` (plus copy display assets to `site/assets/` as established in Study 03) and register in `site/_quarto.yml`, `site/gallery.qmd`, and `site/AGENTS.md`.
2. Run full deterministic verification suite:
   - `Rscript R/proof_lake.R` (28/28)
   - `Rscript -e "mlbr::proof_crosswalks()"` (5/5)
   - `Rscript R/generate_agent_manifest.R` (5/5)
   - `Rscript -e 'devtools::test("pkg/mlbr")'` (all pass)
   - `pytest tests/` (all pass)
   - `quarto render site/` (exit code 0, all pages + Study 04 cleanly built)
   - Confirm all `.rds` artifacts `< 5 MB` and all Gold Parquets ZSTD-compressed.
3. Run independent read-only audits (`sabermetrics-method-auditor` and `silent-failure-hunter`) on the complete diff and resolve every BLOCKER/WARNING before committing and pushing.

---

## 7. Spec Addendum: Enhanced Performance Measurement & Novel Physics Features

To expand how `mlbr` measures true-talent player performance beyond standard box-score or spray-blind summaries, `models/04_player_projections_marcel_aging.R` additionally derives and ablates five higher-resolution physical skill signals strictly from seasons $\le T-1$:

1. **Pulled Air-Ball Rate (`pulled_air_pct`, Batters & Pitchers Allowed, `2015–2026` `v_statcast`):** Share of batted balls in play with `launch_angle >= 10°` pulled to the pull field ($\text{spray\_angle} < -15^\circ$ for RHB, $> +15^\circ$ for LHB, where $\text{spray\_angle} = \text{atan2}(hc\_x - 125.42,\, 198.27 - hc\_y)\cdot\frac{180}{\pi}$; verified $r = 0.932$ overall ($0.962\text{--}0.977$ by handedness group) with FanGraphs `Pull%` and $r = +0.203$ with RHB `wOBA - xwOBA`). Captures systematic pull-side damage skill missed by spray-blind `xwOBA`.
2. **Zone-Chase Selectivity (`zone_chase_selectivity = Z-Swing% - O-Swing%`, SEAGER-inspired box-zone proxy, `2015–2025`):** Separates true pitch-recognition aggression (swinging at strikes while rejecting chases) from raw passivity.
3. **Sweet-Spot Launch-Angle Consistency (`sweet_spot_pct`, `LA ∈ [8°, 32°]`, `2015–2026`):** Quantifies repeatable barrel-plane contact skill.
4. **Fastball Velocity Momentum (`delta_fbv = FBv_{t-1} - FBv_{t-2}`, Pitchers, `2008–2025`):** Captures individual biomechanical velocity gain/loss directly rather than relying solely on chronological age.
5. **Hawk-Eye Bat Tracking Ablation (`M4-BatTrack`, `2023 -> 2024` calibration, locked `2024 -> 2025` OOS):** Tests whether `AvgBatSpeed`, `BlastSwing_pct`, `SquaredUpSwing_pct`, and `SwingLength` add measurable locked-OOS lift on `2025` `wOBA`, `ISO`, and `K%` beyond `xwOBA` + spray/discipline physics.
6. **Exposure-Calibrated 80% Projection Uncertainty Intervals (`lo80`, `hi80`):** Calibrated strictly from out-of-sample historical residuals as $\hat{\sigma}_{\text{total}}(N) = \sqrt{\hat{\sigma}_{\text{skill}}^2 + \hat{\sigma}_{\text{noise}}^2 / N}$ and validated for empirical ~80% coverage on locked `2025` OOS outcomes — providing calibrated variance inputs for Level 4 Monte Carlo simulation.

### 7.1 Methodological Refinements (Validated During Phase 2 Diagnostics)
- **Zero-Sum League Conservation for Delta-Method Aging (`B3`):** Raw within-player Delta-Method aging shifts capture true biological post-peak decay (`-4.99 mph` SP FBv and `+1.85` SP FIP from age 27 to 38 vs. survivor-biased cross-sectional curves), which shifts a returning MLB cohort by `+0.168` mean FIP because retiring veterans are replaced by younger rookies. Following Tom Tango's exact Marcel conservation law, `B3` subtracts the projected-exposure-weighted cohort mean aging delta by role (`age_delta_norm = age_delta_raw - weighted.mean(age_delta_raw, proj_exp)`), strictly preserving the `T-1` league run environment with zero future leakage.
- **Stage-2 Residual Ridge Stacking for Newer Sensor Tiers (`M4+Stuff+`, `M4+BatTrack`):** Because Hawk-Eye `mlbr Stuff+` (out-of-sample from `2024+`) and Hawk-Eye Bat Tracking (`2023+`) have far shorter calibration histories than 8-season Statcast physics (`2016–2024`), re-fitting 18+ collinear features from scratch on a single calibration season causes severe L2 over-shrinkage (`cal_slope` drifting to `1.8–2.2`). Instead, `M4_Statcast_Physics` (trained over expanding 8–9 seasons, `cal_slope ≈ 1.00`) is held fixed as an offset and a compact, zero-mean Stage-2 Residual Ridge learns strictly the orthogonal residual adjustment $\hat{\Delta}_{\text{Ridge}}$ from the 3–4 new sensor variables (`stuff_plus_t1`, `loc_plus_t1`, `pitch_plus_t1`, `tunnel_plus_t1` for pitchers; `batspeed_t1`, `blast_t1`, `sq_swing_t1`, `swing_len_t1` for batters).

### 7.2 Audit-Hardened v2 Methodological Controls
- **Published Tango (2004) `B2` Benchmark:** Direct Step 5 age multiplier (`1 + 0.006*(29 - age)` if `age <= 29`; `1 / (1 + 0.003*(age - 29))` if `age > 29`) followed by Step 6 projected-exposure league rebaselining.
- **Midpoint Delta-Method Spline & Linear Boundary Extrapolation:** Splines fit against exact transition midpoint `age_mid = age - 0.5` (`knots = c(24.5, 30.5)`) with linear marginal-slope extrapolation below age 21 and above age 39 (preventing zero-sum aging sign inversions on age 40+ veterans).
- **Sum-Before-Shrink Multi-Year PVI & Dedicated Sensor Masks:** Pools raw seasonal `PVI_xwoba` / `PVI_csw` across lags weighted by `w_yr * pvi_n_eff` before single-pass shrinkage (`M = 800`), and isolates `Ref_FG_StuffPlus` on its own `has_t1_fg_stuff` availability mask.
- **Single-Pass Live `2027` Bayesian Updates:** Applies Stage-2 residual `Stuff+` updates (centered on training `mu_vec`) and 1-year zero-sum aging strictly once; re-centers 2026 Statcast batter `wOBA`/`OBP`/`SLG`/`ISO` to 2025 FanGraphs scale; retains injured 2026 players with explicit `played_2026` provenance flag.

### 7.3 Level 2.5 Decomposed Pitching+ & Batter Process Engine Integration (Study 05)
- **Markov-Decomposed Pitching+ Priors (`decomp_stuff_plus`, `decomp_loc_plus`, `decomp_pitch_plus`, `decomp_ptun_plus`):**
  Integrates 5-head decomposed physical sensors (`p(called_strike)`, `p(swing)`, `p(whiff | swing)`, `p(foul | contact)`, `E[xwOBA | BIP]`) from Study 05 to isolate pure command and pitcher decision-making independent of base-out variance.
- **Batter Process Engine Priors (`decision_plus`, `bat_control_plus`, `contact_plus`, `batter_process_plus`):**
  Integrates pitch-level swing decisions (take/swing equity relative to count and pitch quality), bat control (squared-up contact rate), and pure contact efficiency from Study 05.
- **Harmonized Sensor Mask Protocol:**
  Both training (`tr_sp_all`) and evaluation cohorts are harmonized on `(has_t1_stuff == 1L | has_t1_decomp == 1L)` ensuring symmetric model coverage across all modern Statcast eras without sensor dropouts.

### 7.4 Full-Spectrum True-Talent WAR Accounting & Defensive Run Isolation
- **Pure Fielding vs Positional Runs:**
  In FanGraphs accounting, $\text{Defense} = \text{Fielding} + \text{Positional}$. To prevent double-counting positional runs while honoring exact FanGraphs WAR accounting, the model extracts pure fielding runs `b.Fielding AS fld_runs` and projects `proj_fld_runs`, `proj_pos_runs`, and `proj_bsr_runs` independently via playing-time-weighted Marcel shrinkage ($M = 1200\text{ PA}$ for fielding, $M = 300\text{ PA}$ for position, $M = 600\text{ PA}$ for baserunning).
- **Exact True-Talent WAR Formulation:**
  $$\text{proj\_def\_runs} = \text{proj\_fld\_runs} + \text{proj\_pos\_runs}$$
  $$\text{proj\_war} = \frac{\text{proj\_wraa} + \text{proj\_bsr\_runs} + \text{proj\_fld\_runs} + \text{proj\_pos\_runs} + \text{proj\_repl\_runs}}{9.77}$$
  $$\text{proj\_repl\_runs} = \left(\frac{\text{proj\_pa}}{600}\right) \times 20.0, \qquad \text{proj\_wraa} = \left(\frac{\text{proj\_woba} - \mu_{\text{lg}}^{\text{wOBA}}}{1.15}\right) \times \text{proj\_pa}$$
- **Pitcher True-Talent WAR:**
  $$\text{proj\_rar} = \left(\mu_{\text{lg}}^{\text{ERA}} - \text{proj\_fip} + \text{Role\_Adj}\right) \times \frac{\text{proj\_ip}}{9.0}, \qquad \text{proj\_war} = \frac{\text{proj\_rar}}{9.77}$$
  where $\text{Role\_Adj} = +0.38\text{ runs/9}$ for Starting Pitchers, $+0.47\text{ runs/9}$ for Relief Pitchers.
