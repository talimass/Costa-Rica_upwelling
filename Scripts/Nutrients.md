```python
import pandas as pd
import numpy as np
from scipy import stats

```


```python
# Load the data (adjust path + separator if needed)
df = pd.read_csv("CR_Nutirents_2025.csv", sep=",")
df.columns = df.columns.str.strip()

print(df.columns)
df.head()

```

    Index(['Site', 'Sample #', 'Nitrate (_M)', 'Phosphate (_M)', 'Si (_M)'], dtype='object')





<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>Site</th>
      <th>Sample #</th>
      <th>Nitrate (_M)</th>
      <th>Phosphate (_M)</th>
      <th>Si (_M)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>Samara 1</td>
      <td>1</td>
      <td>1.3596</td>
      <td>0.2185</td>
      <td>2.5954</td>
    </tr>
    <tr>
      <th>1</th>
      <td>Samara 1</td>
      <td>2</td>
      <td>0.9138</td>
      <td>0.2546</td>
      <td>2.4667</td>
    </tr>
    <tr>
      <th>2</th>
      <td>Samara 1</td>
      <td>3</td>
      <td>0.9699</td>
      <td>0.2274</td>
      <td>2.5518</td>
    </tr>
    <tr>
      <th>3</th>
      <td>Samara 2</td>
      <td>4</td>
      <td>0.3474</td>
      <td>0.0599</td>
      <td>1.2019</td>
    </tr>
    <tr>
      <th>4</th>
      <td>Samara 2</td>
      <td>5</td>
      <td>0.3871</td>
      <td>0.0388</td>
      <td>1.6117</td>
    </tr>
  </tbody>
</table>
</div>




```python
# taking out all negative numbers and making them 0 
nutrient_cols = [
    "Nitrate (_M)",
    "Phosphate (_M)",
    "Si (_M)"
]
df[nutrient_cols] = df[nutrient_cols].clip(lower=0)

```


```python
# Checking that there are no negative numbers
df[nutrient_cols].min()

```




    Nitrate (_M)      0.1180
    Phosphate (_M)    0.0087
    Si (_M)           0.0000
    dtype: float64




```python
# Extract site name (Samara or Papagayo)
df["Region"] = df["Site"].str.split().str[0]

samara = df[df["Region"] == "Samara"]
papagayo = df[df["Region"] == "Papagayo"]

```


```python
def validity_check(var, alpha=0.05):
    sam = df[df["Region"] == "Samara"][var].dropna()
    pap = df[df["Region"] == "Papagayo"][var].dropna()

    print(f"\n==============================")
    print(f" Validity checks for: {var}")
    print(f"==============================")

    # --- Normality ---
    sh_s = stats.shapiro(sam)
    sh_p = stats.shapiro(pap)

    print(f"Shapiro–Wilk (Samara):   p = {sh_s.pvalue:.4f}  → {'PASS' if sh_s.pvalue > alpha else 'FAIL'}")
    print(f"Shapiro–Wilk (Papagayo): p = {sh_p.pvalue:.4f}  → {'PASS' if sh_p.pvalue > alpha else 'FAIL'}")

    normal = (sh_s.pvalue > alpha) and (sh_p.pvalue > alpha)

    # --- Homogeneity of variance ---
    lev = stats.levene(sam, pap)

    print(f"Levene’s test:           p = {lev.pvalue:.4f}  → {'PASS' if lev.pvalue > alpha else 'FAIL'}")

    equal_var = lev.pvalue > alpha

    # --- Summary ---
    print("\n--- SUMMARY ---")
    print(f"Normality: {'PASS' if normal else 'FAIL'}")
    print(f"Equal variance: {'PASS' if equal_var else 'FAIL'}")

    # --- Recommended test ---
    if normal and equal_var:
        test = "Independent t-test"
    elif normal:
        test = "Welch’s t-test"
    else:
        test = "Mann–Whitney U"

    print(f"Recommended test: {test}")

```


```python
variables = [
    "Nitrate (_M)",
    "Phosphate (_M)",
    "Si (_M)"
]

for v in variables:
    validity_check(v)


```

    
    ==============================
     Validity checks for: Nitrate (_M)
    ==============================
    Shapiro–Wilk (Samara):   p = 0.0004  → FAIL
    Shapiro–Wilk (Papagayo): p = 0.0068  → FAIL
    Levene’s test:           p = 0.1344  → PASS
    
    --- SUMMARY ---
    Normality: FAIL
    Equal variance: PASS
    Recommended test: Mann–Whitney U
    
    ==============================
     Validity checks for: Phosphate (_M)
    ==============================
    Shapiro–Wilk (Samara):   p = 0.0439  → FAIL
    Shapiro–Wilk (Papagayo): p = 0.0601  → PASS
    Levene’s test:           p = 0.4674  → PASS
    
    --- SUMMARY ---
    Normality: FAIL
    Equal variance: PASS
    Recommended test: Mann–Whitney U
    
    ==============================
     Validity checks for: Si (_M)
    ==============================
    Shapiro–Wilk (Samara):   p = 0.0040  → FAIL
    Shapiro–Wilk (Papagayo): p = 0.0004  → FAIL
    Levene’s test:           p = 0.5915  → PASS
    
    --- SUMMARY ---
    Normality: FAIL
    Equal variance: PASS
    Recommended test: Mann–Whitney U



```python
from scipy import stats

def compare_with_validity(var, alpha=0.05):
    sam = df[df["Region"] == "Samara"][var].dropna()
    pap = df[df["Region"] == "Papagayo"][var].dropna()

    print(f"\n==============================")
    print(f" {var}")
    print(f"==============================")

    # --- Shapiro–Wilk (normality) ---
    sh_s = stats.shapiro(sam)
    sh_p = stats.shapiro(pap)

    shapiro_pass = (sh_s.pvalue > alpha) and (sh_p.pvalue > alpha)

    print(f"Shapiro Samara:   p = {sh_s.pvalue:.4f} → {'PASS' if sh_s.pvalue > alpha else 'FAIL'}")
    print(f"Shapiro Papagayo: p = {sh_p.pvalue:.4f} → {'PASS' if sh_p.pvalue > alpha else 'FAIL'}")

    # --- Levene (equal variances) ---
    lev = stats.levene(sam, pap)
    levene_pass = lev.pvalue > alpha

    print(f"Levene:           p = {lev.pvalue:.4f} → {'PASS' if levene_pass else 'FAIL'}")

    # --- Choose test ---
    if shapiro_pass and levene_pass:
        test_name = "Independent t-test"
        stat, pval = stats.ttest_ind(sam, pap, equal_var=True)
    else:
        test_name = "Mann–Whitney U"
        stat, pval = stats.mannwhitneyu(sam, pap, alternative="two-sided")

    # --- Results ---
    print(f"\nTest used: {test_name}")
    print(f"Statistic: {stat:.3f}")
    print(f"p-value:   {pval:.4f}")

nutrients = [
    "Nitrate (_M)",
    "Phosphate (_M)",
    "Si (_M)"
]

for n in nutrients:
    compare_with_validity(n)


```

    
    ==============================
     Nitrate (_M)
    ==============================
    Shapiro Samara:   p = 0.0004 → FAIL
    Shapiro Papagayo: p = 0.0068 → FAIL
    Levene:           p = 0.1344 → PASS
    
    Test used: Mann–Whitney U
    Statistic: 143.000
    p-value:   0.7863
    
    ==============================
     Phosphate (_M)
    ==============================
    Shapiro Samara:   p = 0.0439 → FAIL
    Shapiro Papagayo: p = 0.0601 → PASS
    Levene:           p = 0.4674 → PASS
    
    Test used: Mann–Whitney U
    Statistic: 107.000
    p-value:   0.3201
    
    ==============================
     Si (_M)
    ==============================
    Shapiro Samara:   p = 0.0040 → FAIL
    Shapiro Papagayo: p = 0.0004 → FAIL
    Levene:           p = 0.5915 → PASS
    
    Test used: Mann–Whitney U
    Statistic: 258.000
    p-value:   0.0000



```python
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import matplotlib.patches as mpatches
from scipy.stats import mannwhitneyu

# --------------------------
#   GLOBAL FONT SETTINGS
# --------------------------
plt.rcParams.update({
    "font.family": "Times New Roman",
    "font.size": 26,
    "axes.titlesize": 32,
    "axes.labelsize": 28,
    "xtick.labelsize": 26,
    "ytick.labelsize": 26,
    "legend.fontsize": 26
})

# --------------------------
#   LOAD DATA
# --------------------------
data = {
    "Site": [
        "Samara","Samara","Samara","Samara","Samara","Samara","Samara","Samara","Samara",
        "Samara","Samara","Samara","Samara","Samara","Samara",
        "Papagayo","Papagayo","Papagayo","Papagayo","Papagayo","Papagayo",
        "Papagayo","Papagayo","Papagayo","Papagayo","Papagayo","Papagayo",
        "Papagayo","Papagayo","Papagayo","Papagayo","Papagayo","Papagayo"
    ],
    "Nitrate": [
        1.3596,0.9138,0.9699,0.3474,0.3871,0.5890,0.6307,0.6452,0.5870,
        0.7411,0.5857,0.6141,2.7241,2.7183,2.6130,
        0.1763,0.1881,0.1180,0.4740,0.3271,0.4553,
        1.1043,1.3484,1.1522,0.6808,0.2460,0.3494,
        3.3057,3.2039,3.4541,2.3385,2.2461,2.3853
    ],
    "Phosphate": [
        0.2185,0.2546,0.2274,0.0599,0.0388,0.0087,0.0384,0.0293,0.0604,
        0.1019,0.1059,0.1273,0.3114,0.3287,0.3247,
        0.1009,0.0906,0.1322,0.1009,0.0998,0.0781,
        0.2336,0.2278,0.2532,0.0405,0.0164,0.0393,
        0.3741,0.3775,0.3765,0.3210,0.3045,0.2662
    ],
    "Silicate": [
        2.5954,2.4667,2.5518,1.2019,1.6117,1.0153,1.5974,1.5410,1.6599,
        1.6334,1.6418,1.6166,2.8301,5.0873,3.0448,
        0,0,0,0,0,0,
        0.4557,0.5727,0.5604,0,0,0,
        1.2986,1.2710,1.5303,1.3507,1.2761,1.3576
    ]
}

df = pd.DataFrame(data)

# --------------------------
#   RESHAPE TO LONG FORMAT
# --------------------------
df_long = df.melt(
    id_vars="Site",
    value_vars=["Nitrate", "Phosphate", "Silicate"],
    var_name="Nutrient",
    value_name="Concentration"
)

# --------------------------
#   FIGURE SETUP (1x3)
# --------------------------
fig, axes = plt.subplots(1, 3, figsize=(22, 9))
axes = axes.flatten()

station_order = [ "Papagayo","Samara"]
station_colors = {"Samara": "#E69F00", "Papagayo": "#0072B2"}

# --------------------------
#   MAIN PLOTTING LOOP
# --------------------------
for ax, nutrient in zip(axes, ["Nitrate", "Phosphate", "Silicate"]):

    sns.boxplot(
        data=df_long[df_long["Nutrient"] == nutrient],
        x="Site",
        y="Concentration",
        order=station_order,
        palette=station_colors,
        ax=ax,
        saturation=1,
        width=0.6,
        showfliers=True
    )

    ax.set_title(nutrient, pad=12)
    ax.set_xlabel("")
    ax.set_ylabel("Concentration (μM)", labelpad=15)

# --------------------------
#   SIGNIFICANCE TEST (Silicate)
# --------------------------
sil_samara = df[df["Site"] == "Samara"]["Silicate"]
sil_papagayo = df[df["Site"] == "Papagayo"]["Silicate"]

u_stat, p_val = mannwhitneyu(sil_samara, sil_papagayo, alternative="two-sided")

ax_sil = axes[2]
ymin, ymax = ax_sil.get_ylim()
headroom = (ymax - ymin) * 0.25
ax_sil.set_ylim(ymin, ymax + headroom)

star = "***" if p_val < 0.001 else "**" if p_val < 0.01 else "*" if p_val < 0.05 else "ns"

ax_sil.text(0.5, ymax + headroom * 0.35, star,
            ha="center", va="center", fontsize=32, fontweight="bold")

ax_sil.plot([0, 1], [ymax + headroom * 0.25, ymax + headroom * 0.25],
            lw=2, color="black")

# --------------------------
#   LEGEND
# --------------------------
sam_patch = mpatches.Patch(color=station_colors["Samara"], label="Samara")
pap_patch = mpatches.Patch(color=station_colors["Papagayo"], label="Papagayo")

fig.legend(
    handles=[sam_patch, pap_patch],
    loc="lower center",
    ncol=2,
    fontsize=28,
    title="Location",
    title_fontsize=30,
    frameon=True,
    bbox_to_anchor=(0.5, -0.05)
)

plt.subplots_adjust(wspace=0.35, bottom=0.25)

# --------------------------
#   SAVE
# --------------------------
plt.savefig("Nutrients_Boxplot_Samara_Papagayo.png", dpi=600, bbox_inches="tight")
plt.show()

print(f"Silicate Mann–Whitney p-value = {p_val:.4e}")

```

    /var/folders/b4/_jmnd3zs6vdcbnjlbry4mf340000gn/T/ipykernel_41583/1542514715.py:81: FutureWarning: 
    
    Passing `palette` without assigning `hue` is deprecated and will be removed in v0.14.0. Assign the `x` variable to `hue` and set `legend=False` for the same effect.
    
      sns.boxplot(
    /var/folders/b4/_jmnd3zs6vdcbnjlbry4mf340000gn/T/ipykernel_41583/1542514715.py:81: FutureWarning: 
    
    Passing `palette` without assigning `hue` is deprecated and will be removed in v0.14.0. Assign the `x` variable to `hue` and set `legend=False` for the same effect.
    
      sns.boxplot(
    /var/folders/b4/_jmnd3zs6vdcbnjlbry4mf340000gn/T/ipykernel_41583/1542514715.py:81: FutureWarning: 
    
    Passing `palette` without assigning `hue` is deprecated and will be removed in v0.14.0. Assign the `x` variable to `hue` and set `legend=False` for the same effect.
    
      sns.boxplot(



    
![png](output_8_1.png)
    


    Silicate Mann–Whitney p-value = 7.6739e-06



```python

```
