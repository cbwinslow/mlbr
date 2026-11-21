# Specification 08: Level 3 Team Run Creation, BaseRuns & Pythagenpat Standings Simulator

- **Experiment ID:** `EXP-008-TEAM-BASERUNS-PYTHAGENPAT`
- **Architecture Tier:** Level 3 (Team Run Creation/Prevention & Dynamic Standings Engine)
- **Status:** `VALIDATED`
- **Upstream Dependencies:**
  - Gold Tier: `data/processed/ballpark_factors_2002_2025.parquet` (`EXP-006`)
  - Gold Tier: `data/processed/pitcher_projections_true_talent.parquet` (`EXP-004`)
  - Gold Tier: `data/processed/batter_projections_true_talent.parquet` (`EXP-004`)
  - Bronze/Silver Views: `v_retrosheet_games` (`2002–2025`), `v_retrosheet_plays` (`2002–2025`), `v_mlbmeta_schedule` (`2002–2026`)
- **Downstream Consumers:** Level 4 Plate-Appearance Monte Carlo Game Simulator, Level 5 Prediction Market Valuation (Season Win Totals, Division Titles, Playoff Odds).

---

## 1. Mathematical Objective & Theoretical Grounding

### 1.1 The Run-Creation Non-Linearity Problem
Linear weights models (e.g. wOBA, Extrapolated Runs, Batting Runs) assume that each event adds a fixed increment of runs regardless of the surrounding context. However, baseball run scoring is intrinsically non-linear and interactive:
$$\text{Runs} \propto (\text{Base Runners}) \times (\text{Advancement Power})$$
A single has near-zero run value with no runners on base and two outs, but enormous value with runners in scoring position. Teams with high on-base percentages convert subsequent extra-base hits into multiple runs at super-linear rates.

In 1993–1994, **David Smyth** formulated **BaseRuns**, the most mathematically rigorous and robust non-linear run estimator in sabermetric history:
$$\text{BaseRuns} = \frac{A \cdot B}{B + C} + D$$
where:
- **$A$ (Base Runners on Base):** The volume of runners who reach base safely without scoring immediately on their own batted ball:
  $$A = H + BB + HBP - HR - 0.5 \cdot IBB$$
- **$B$ (Advancement & Scoring Rate):** The ability of the lineup to advance runners around the bases:
  $$B = (1.4 \cdot TB - 0.6 \cdot H - 3 \cdot HR + 0.1 \cdot (BB + HBP - IBB) + 0.9 \cdot (SB - CS - GDP)) \times 1.1$$
- **$C$ (Outs Remaining / Inning Clocks):** The capacity of the defense/pitching to record outs:
  $$C = AB - H + CS + GDP$$
- **$D$ (Guaranteed Self-Scored Runs):** Home runs that score automatically regardless of baserunners:
  $$D = HR$$

### 1.2 Pythagenpat Dynamic Exponent for Expected Wins
Bill James' original Pythagorean expectation used a fixed exponent ($\gamma = 2.0$, later tuned by Tango/Davenport to $\gamma = 1.83$):
$$W\% = \frac{R^\gamma}{R^\gamma + RA^\gamma}$$
However, fixed exponents fail across extreme run environments. A 100-run differential in a low-scoring environment (e.g. $600\text{ R}$ vs. $500\text{ RA}$, $+100$) represents a substantially larger win probability advantage than the same $+100$ differential in a high-scoring environment (e.g. $1000\text{ R}$ vs. $900\text{ RA}$ at Coors Field).

**David Smyth and Patriot (2003)** solved this by deriving **Pythagenpat**, where the exponent $\gamma$ is a continuous, dynamic function of the total game scoring environment:
$$\gamma = \left(\frac{R + RA}{G}\right)^{0.287}$$
This dynamic exponent ensures exact calibration across all eras (Dead-ball, 1990s Steroid era, 2019 Rabbit ball, modern run environments).

---

## 2. Input Features & Data Lake Extraction

The engine extracts team-season scoring totals from `v_retrosheet_games` and event-level component totals from `v_retrosheet_plays` (`BAT_EVENT_FL = 'T'`) across 2002–2025:
```sql
SELECT
  TRY_CAST(SUBSTRING(g.GAME_ID, 4, 4) AS INTEGER) AS season,
  t.team_id,
  COUNT(DISTINCT g.GAME_ID) AS g,
  SUM(t.runs_scored) AS r,
  SUM(t.runs_allowed) AS ra,
  SUM(t.w) AS wins,
  SUM(t.l) AS losses,
  -- Component totals for BaseRuns A, B, C, D
  SUM(t.singles) AS singles,
  SUM(t.doubles) AS doubles,
  SUM(t.triples) AS triples,
  SUM(t.hr) AS hr,
  SUM(t.bb) AS bb,
  SUM(t.hbp) AS hbp,
  SUM(t.so) AS so,
  SUM(t.sb) AS sb,
  SUM(t.cs) AS cs,
  SUM(t.outs) AS outs
FROM ...
```

---

## 3. Methodological Workflow & Validation Strategy

1. **Phase 1: Historical BaseRuns vs. Actual Runs Validation (2002–2025):**
   - Compute offensive BaseRuns ($BsR_{\text{off}}$) and defensive BaseRuns allowed ($BsR_{\text{def}}$) for all 720 team-seasons.
   - Benchmark RMSE, MAE, and correlation $r$ against actual runs scored ($R$) and runs allowed ($RA$).
   - Confirm BaseRuns achieves $r > 0.985$ with actual scoring.

2. **Phase 2: Pythagenpat vs. Classical Pythagorean Backtest (2002–2025):**
   - Compare actual win totals against:
     - $B_0$: 81-81 neutral baseline
     - $B_1$: Prior-year win percentage
     - $B_2$: Fixed Pythagorean ($\gamma = 1.83$)
     - $M_3$: Dynamic Pythagenpat on Actual Runs
     - $M_4$: Dynamic Pythagenpat on BaseRuns True-Talent Runs
   - Quantify "Luck / Sequencing Residual" ($W - W_{\text{Pyth}}$) and test its year-over-year persistence ($r \approx 0.0$, confirming sequencing luck does not persist).

3. **Phase 3: Ballpark-Adjusted BaseRuns Integration:**
   - Adjust team expected runs using Gold 3-year regressed ballpark factors from `EXP-006`:
     $$R_{\text{neutral}} = \frac{R_{\text{home}}}{PF_{\text{runs}} / 100} + R_{\text{road}}$$
   - Separate team offensive talent from home ballpark inflation.

4. **Phase 4: Level 2 Roster Aggregation & 2026 Standings Simulation:**
   - Map projected 2026 player true talent from `EXP-004` into depth-chart playing-time distributions across all 30 MLB clubs.
   - Aggregate projected player BaseRuns into projected team $R_{2026}$ and $RA_{2026}$.
   - Run 10,000 Monte Carlo schedule simulations over `v_mlbmeta_schedule` for 2026.
   - Compute projected division win probabilities, wild-card probabilities, and 80% win-total credible intervals.

---

## 4. Acceptance Criteria & Guardrails
1. **Mathematical Monotonicity:** $BsR(A, B, C, D) \ge 0$ strictly for all valid non-negative inputs.
2. **Inversion & Outlier Bounds:** $|BsR - R| \le 45\text{ runs}$ across all 720 historical team-seasons (95% within 20 runs).
3. **Dynamic Exponent Invariant:** For standard MLB run environments ($8.0 \le \frac{R+RA}{G} \le 10.5$), $\gamma \in [1.80, 1.95]$.
4. **Lake Parity:** Companion `workbooks/lab07_team_baseruns_pythagenpat_workbook.R` and `site/workbooks/lab07_team_baseruns_pythagenpat_workbook.qmd` with 3+ mentor checkpoints.
5. **Asset Preservation:** 4-asset suites (`.png` 300 DPI, `.rds` $< 5.0\text{ MB}$, `.parquet`, `.html`).

