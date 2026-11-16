# Spec 01: Atomic Pitch Quality, Approach Angles & xWhiff Model
**Framework:** Q-CRISP (Quantitative CRISP-DM)  
**Level:** Level 1 (Atomic Physics & Tracked Quality)  
**Target Variable:** Swing-and-Miss (`is_whiff`)  
**Data Universe:** 100% Full Population (Statcast 2008–2026, 14.64M pitches)  

---

## 1. Domain Problem & Mathematical Motivation
Traditional box-score metrics (ERA, WHIP, Strikeouts per 9) conflate a pitcher's intrinsic pitch execution with defense, ballpark geometry, umpire bias, and sequencing luck. 

To evaluate pure "Stuff" and predict game-level strikeout rates without noise, we isolate the physics of each pitch at the moment it reaches the batter.

### Theoretical Aerodynamics:
1. **Vertical Approach Angle (VAA)**: The angle at which the pitch crosses the front edge of home plate ($y = 1.417\text{ ft}$).
   $$v_{yf} = -\sqrt{v_{y0}^2 + 2 a_y (1.417 - 50)}$$
   $$t_{\text{flight}} = \frac{v_{yf} - v_{y0}}{a_y}$$
   $$v_{zf} = v_{z0} + a_z t_{\text{flight}}, \quad v_{xf} = v_{x0} + a_x t_{\text{flight}}$$
   $$\text{VAA} = \arctan\left(\frac{v_{zf}}{-v_{yf}}\right) \times \left(\frac{180}{\pi}\right)$$
   $$\text{HAA} = \arctan\left(\frac{v_{xf}}{-v_{yf}}\right) \times \left(\frac{180}{\pi}\right)$$
2. **$\text{VAA}_{\text{diff}}$ (VAA Above Expected)**:
   Expected VAA is a function of vertical plate location:
   $$\mathbb{E}[\text{VAA} \mid \text{plate\_z}] \approx \alpha + \beta \cdot \text{plate\_z}$$
   $$\text{VAA}_{\text{diff}} = \text{VAA} - \mathbb{E}[\text{VAA} \mid \text{plate\_z}]$$
   Positive $\text{VAA}_{\text{diff}}$ on fastballs creates the visual optical illusion of "rise," inducing swings underneath the ball.
3. **Fastball Velocity Differential ($\Delta V$)**:
   $$\Delta V = \bar{V}_{\text{pitcher, fastball}} - V_{\text{pitch}}$$
   Quantifies timing disruption on offspeed and breaking pitches.
4. **Platoon-Normalized Plate Coordinates**:
   $$\text{plate\_x\_norm} = \text{plate\_x} \times \begin{cases} 1 & \text{if stand = 'R'} \\ -1 & \text{if stand = 'L'} \end{cases}$$

---

## 2. Full-Population DuckDB Feature Pipeline
Features are computed across all 14.64 million pitches using vectorized SIMD math in DuckDB:

```sql
CREATE OR REPLACE VIEW v_statcast_features AS
WITH base AS (
  SELECT 
    *,
    -SQRT(vy0*vy0 + 2 * ay * (1.417 - 50.0)) AS vy_f,
    (-SQRT(vy0*vy0 + 2 * ay * (1.417 - 50.0)) - vy0) / ay AS t_flight
  FROM v_statcast
  WHERE vy0 IS NOT NULL AND ay IS NOT NULL
),
angles AS (
  SELECT 
    *,
    vz0 + az * t_flight AS vz_f,
    vx0 + ax * t_flight AS vx_f,
    ATAN2(vz0 + az * t_flight, -vy_f) * (180.0 / PI()) AS vaa,
    ATAN2(vx0 + ax * t_flight, -vy_f) * (180.0 / PI()) AS haa,
    plate_x * (CASE WHEN stand = 'L' THEN -1.0 ELSE 1.0 END) AS plate_x_norm,
    CASE 
      WHEN abs(plate_x) <= 0.55 AND plate_z BETWEEN 1.83 AND 3.17 THEN 'Heart'
      WHEN abs(plate_x) <= 1.11 AND plate_z BETWEEN 1.25 AND 3.75 THEN 'Shadow'
      WHEN abs(plate_x) <= 1.66 AND plate_z BETWEEN 0.67 AND 4.33 THEN 'Chase'
      ELSE 'Waste'
    END AS attack_zone,
    CASE WHEN description IN ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') THEN 1 ELSE 0 END AS is_whiff,
    CASE WHEN description IN ('swinging_strike', 'swinging_strike_blocked', 'foul_tip', 'foul', 'hit_into_play') THEN 1 ELSE 0 END AS is_swing
  FROM base
)
SELECT * FROM angles;
```

---

## 3. Modeling & Temporal Validation Strategy
* **Temporal Split (Zero Look-Ahead Leakage)**:
  * Training Cohort: Seasons 2018–2023
  * Validation Cohort: Season 2024
  * Out-of-Sample Test Cohort: Seasons 2025–2026
* **Candidate Algorithms**:
  1. Baseline: Logistic Regression (`glm`)
  2. Non-linear Tree: Multithreaded Random Forest (`ranger`, 500 trees)
  3. Gradient Boosting: `xgboost` with monotonic constraints on velocity
* **Evaluation Metrics**:
  * Out-of-fold Log-Loss
  * Brier Score Calibration ($BS = \frac{1}{N} \sum (f_t - o_t)^2$)
  * ROC AUC & PR AUC

---

## 4. Deliverables
1. `models/01_eda_feature_expansion.R` — Interactive RStudio tutorial and visualizer.
2. `site/analyses/01_pitch_quality_eda.qmd` — Peer-reviewed publication page on Quarto website.
3. `pkg/mlbr/R/vaa.R` — Exported R package functions (`calculate_vaa`, `calculate_attack_zones`).
