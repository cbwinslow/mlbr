# Quantitative Experiment & Knowledge Base Registry

This curated registry catalogs all verified mathematical models, empirical formulas, invariants, and rejected hypotheses across the 5-tier predictive architecture.

| Experiment ID | Tier | Title | Status | Key Formula / Metric | Updated |
|:---|:---:|:---|:---:|:---|:---|
| **[EXP-001-AERODYNAMICS](registry.json)** | 1 | Trajectory Aerodynamics, Approach Angles (VAA/HAA) & Whiff Lift | `VALIDATED` | `terminal_forward_velo`: vy_f = -sqrt(vy0^2 + 2 * ay * (1... | 2026-11-16 |
| **[EXP-002-PLATOON-PHYSICS](registry.json)** | 1 | 4-Quadrant Platoon Physics, Break Symmetry & Situational Pitching Effectiveness | `VALIDATED` | `hb_glove_in`: hb_in * (CASE WHEN p_throws = 'L' THEN -1 ... | 2026-11-18 |
| **[EXP-003-TUNNELING-STUFF-PLUS](registry.json)** | 1 | Commit-Point Pitch Tunneling & Platoon-Conditioned Stuff+ / Location+ / Pitching+ | `VALIDATED` | `commit_time_t_dec`: (-sqrt(vy0^2 + 2*ay*(23.8 - 50.0)) -... | 2026-11-19 |
| **[EXP-005-DECOMPOSED-PITCHING-DECISIONS](registry.json)** | 1 | Context-Neutral Markov Decomposed Pitching+ & Pitch-Level Batter Process Engine (Decision+, BatControl+, Contact+) | `VALIDATED` | `bip_rv_map`: delta_run_exp ~ factor(count_id) + 0.8593 *... | 2026-11-20 |
| **[EXP-011-BAT-TRACKING-ARM-SLOT-BIOMECHANICS](registry.json)** | 1 | Hawk-Eye Bat-Tracking (1.15M Swings) & Pitcher Arm-Slot (arm_angle) Biomechanics | `VALIDATED` | `max_ev_collision`: MaxEV = 1.23 * bat_speed + 0.2116 * r... | 2026-11-22 |
| **[EXP-021-PITCH-SEQUENCING-ENTROPY-MARKOV](registry.json)** | 1 | Pitch Sequencing, First-Order Markov Transitions, Eye-Level Shifts & Pitcher Shannon Entropy (Seq+) | `VALIDATED` | `shannon_entropy`: H_count(p,s) = sum_c w_{p,s,c} * (-sum... | 2026-11-23 |
| **[EXP-004-PLAYER-PROJECTIONS](registry.json)** | 2 | Chronological True-Talent Player Projections: Marcel, Delta-Method Aging & Tier 1 Physics Priors | `VALIDATED` | `exact_ip_true`: FLOOR(IP) + ROUND((IP - FLOOR(IP)) * 10)... | 2026-11-20 |
| **[EXP-007-PECOTA-ZIPS-ENSEMBLE](registry.json)** | 2 | PECOTA k-NN Similarity Comps, ZiPS Archetype Multi-Year Aging, Defense/Baserunning EDA & 5-Paradigm Consensus Ensemble | `VALIDATED` | `era_neutral_zscore`: z_{i,s} = (x_{i,s} - mu_{s,g}) / sd... | 2026-11-21 |
| **[EXP-024-SPRAY-ANGLE-PARK-WALL-XHR](registry.json)** | 2 | Batted-Ball Spray-Angle, Pull-Air% & Ballpark Wall-Geometry xHR Physics | `VALIDATED` | m0_oos_logloss: 0.06129 | 2026-11-23 |
| **[EXP-006-BALLPARK-FACTORS](registry.json)** | 3 | Empirical Multi-Year Regressed Ballpark Factors & Handedness Asymmetry | `VALIDATED` | `raw_scoring_ratio`: (r_home / g_home) / (r_road / g_road) | 2026-11-20 |
| **[EXP-008-TEAM-BASERUNS-PYTHAGENPAT](registry.json)** | 3 | Level 3 Team Run Creation (David Smyth BaseRuns), Dynamic Pythagenpat & 10,000-Season 2026 Standings Simulator | `VALIDATED` | `david_smyth_baseruns`: R = (A * B) / (B + C) + D | 2026-11-21 |
| **[EXP-012-WEATHER-UMPIRE-CATCHER-FRAMING](registry.json)** | 3 | Environmental & Officiating Alpha: Weather/Wind Carry, Home-Plate Umpire Strike Zones & Catcher Framing (CSAA) | `VALIDATED` | `weather_carry`: R_env = b0 + b1*PF_runs + b2*(Temp - 70)... | 2026-11-22 |
| **[EXP-013-PITCHER-STAMINA-BULLPEN-FATIGUE](registry.json)** | 3 | Pitcher Stamina, In-Game Fastball Velocity Decay & Dynamic Rolling Bullpen Fatigue Engine | `VALIDATED` | `velo_decay`: Delta_v(k) = v(k) - mean(v_1..20) | 2026-11-22 |
| **[EXP-022-REST-IL-RECOVERY-TRAVEL-JETLAG](registry.json)** | 3 | Starter Rest-Day Recovery, Post-IL Return Pitch-Count/Velocity Ramp & Circadian Travel Jet-Lag | `VALIDATED` | `within_pitcher_fe`: Delta_y_{p,s,t} = y_{p,s,t} - mean(y... | 2026-11-23 |
| **[EXP-009-GAME-MONTE-CARLO-SIMULATOR](registry.json)** | 4 | Level 4 Plate-Appearance 24 Base-Out Markov Game Simulator & Empirical Win Expectancy Engine | `VALIDATED` | `multi_category_log5`: P(E) = (P_bat(E) * P_pit(E) / P_lg... | 2026-11-21 |
| **[EXP-023-LIVE-BAYESIAN-WIN-PROB-LEVERAGE](registry.json)** | 4 | Live In-Game Bayesian Win Probability, Pre-Game Prior Decay & Real-Time Leverage Index | `VALIDATED` | gamma_decay_opt: 0.15 | 2026-11-23 |
| **[EXP-010-MARKET-EDGE-PREDICTION-VALUATION](registry.json)** | 5 | Level 5 Market Edge, Closing Line Value (CLV), Fractional Kelly Bankroll Optimization & Prediction Market Valuation | `VALIDATED` | `expected_value`: EV = p * (O - 1) - (1 - p) = p * O - 1 | 2026-11-22 |
| **[EXP-014-UNIFIED-PRO-GAME-F5-TOTALS-BRACKET](registry.json)** | 5 | Unified Pro Multi-Market Engine (Full-Game ML, F5 ML, Totals) & Exact 12-Team MLB Playoff Bracket Simulator | `VALIDATED` | `unified_ml`: logit(p_ML) = b0 + b1*logit(p_M3) + b2*logi... | 2026-11-22 |
| **[EXP-015-DAILY-LINEUPS-PLATOON-K-PROPS](registry.json)** | 5 | Daily 9-Man Starting Lineups, Handedness Park Asymmetry & Starting Pitcher Strikeout (SO) Prop Pricing Engine | `VALIDATED` | `lineup_k_pct`: LineupK% = sum(w_slot * EB_K%_prior) / su... | 2026-11-22 |
| **[EXP-016-DYNAMIC-STATESPACE-NEGBIN-K-PROPS](registry.json)** | 5 | Dynamic In-Season State-Space Talent Updates (30d CSW%, xwOBA, Silver Bat Speed) & Negative Binomial Overdispersed K-Props | `VALIDATED` | `negbin_var`: Var(SO) = mu + mu^2 / theta | 2026-11-22 |
| **[EXP-017-BATTER-HR-TOTAL-BASES-PROPS](registry.json)** | 5 | Batter Home Run (HR >= 1) & Total Bases (TB >= 2) Prop Pricing Engine via v_statcast_biomechanics | `VALIDATED` | `hr_prop_logit`: logit(P(HR>=1)) = b0 + b1*EB_ISO + b2*EB... | 2026-11-22 |
| **[EXP-018-SGP-CORRELATION-COPULA-PRICING](registry.json)** | 5 | Same-Game Parlay (SGP) Correlation Matrix & Gaussian/Clayton Copula Pricing Engine | `VALIDATED` | `sklar_copula`: P(Y1=1, Y2=1) = C(p1, p2; psi), constrain... | 2026-11-22 |
| **[EXP-019-ORDERED-TB-EXP-DECAY-STATESPACE](registry.json)** | 5 | Joint Ordered/Multinomial Total Bases Ladder (0-4+ TB) & Exponential Half-Life State-Space Decay | `VALIDATED` | `ordered_polr`: logit(P(TB <= k | x)) = alpha_k - beta'x,... | 2026-11-22 |
| **[EXP-020-DYNAMIC-WORKLOAD-MULTIVARIATE-KELLY](registry.json)** | 5 | Interior Half-Life Sweep, Dynamic In-Season Starter Workload (BF/GS) & Multivariate Copula-Kelly Staking | `VALIDATED` | `dyn_bf_gs`: BF_GS_H(p, t) = (sum_{d<t} 2^(-dt/H)*BF_{p,d... | 2026-11-23 |

---
*Maintained by mlbr::lake_log_experiment()*
