```python
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import numpy as np


# LOAD DATA

df = pd.read_csv("Corals.csv")


```


```python
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np

# --------------------------
# LOAD DATA
# --------------------------
df = pd.read_csv("Corals.csv")

# --------------------------
# CREATE LOCATION GROUPS
# --------------------------
df['Location'] = df['Site'].apply(
    lambda x: 'Papagayo' if 'Papagayo' in x else 'Samara'
)

# --------------------------
# CORAL COLUMNS
# --------------------------
coral_cols = [
    'Pavona',
    'Pocillopora effusa',
    'Porites lobata',
    'Psammocora stellata'
]

# --------------------------
# COLORS (FULL SATURATION)
# --------------------------
palette = {
    'Samara': '#E69F00',
    'Papagayo': '#0072B2'
}

# --------------------------
# GLOBAL FONT SETTINGS
# --------------------------
plt.rcParams.update({
    "font.family": "Times New Roman",
    "font.size": 26,
    "axes.titlesize": 32,
    "axes.labelsize": 28,
    "ytick.labelsize": 26,
    "legend.fontsize": 26
})

# --------------------------
# CREATE SUBPLOTS (2x2)
# --------------------------
fig, axes = plt.subplots(2, 2, figsize=(16, 12))
axes = axes.flatten()

# --------------------------
# LOOP THROUGH CORALS
# --------------------------
for i, coral in enumerate(coral_cols):

    ax = axes[i]

    # --------------------------
    # REMOVE ZERO VALUES
    # --------------------------
    plot_df = df[df[coral] > 0].copy()

    if len(plot_df) == 0:
        ax.set_visible(False)
        continue

    # --------------------------
    # DYNAMIC Y LIMIT
    # --------------------------
    upper = np.percentile(plot_df[coral], 95)
    y_max = upper * 1.25

    # --------------------------
    # BOXPLOT
    # --------------------------
    sns.boxplot(
        data=plot_df,
        x='Location',
        y=coral,
        hue='Location',
        dodge=False,
        palette=palette,
        width=0.6,
        linewidth=2.5,
        showfliers=True,
        saturation=1,
        ax=ax
    )

    # --------------------------
    # REMOVE LEGEND
    # --------------------------
    if ax.get_legend() is not None:
        ax.get_legend().remove()

    # --------------------------
    # REMOVE AXIS LABELS
    # --------------------------
    ax.set_xlabel('')
    ax.set_ylabel('')

    # --------------------------
    # REMOVE X-AXIS TICK LABELS
    # --------------------------
    ax.set_xticklabels([])

    # --------------------------
    # REMOVE X-AXIS TICK MARKS
    # --------------------------
    ax.tick_params(
        axis='x',
        which='both',
        bottom=False,
        top=False,
        length=0,
        labelbottom=False
    )

    # --------------------------
    # Y TICKS STYLING
    # --------------------------
    ax.tick_params(
        axis='y',
        which='major',
        length=8,
        width=2,
        labelsize=26
    )

    # --------------------------
    # TITLE
    # --------------------------
    ax.set_title(coral, fontsize=32, pad=15)

    # --------------------------
    # Y LIMITS
    # --------------------------
    ax.set_ylim(0, y_max)

    #Astriks
    ax.text(
    0.5, 0.95,
    '***',
    transform=ax.transAxes,
    ha='center',
    va='top',
    fontsize=36,
    fontweight='bold'
)
    # --------------------------
    # FULL BORDER
    # --------------------------
    for spine in ax.spines.values():
        spine.set_visible(True)
        spine.set_linewidth(2.5)

# --------------------------
# SPACING
# --------------------------
plt.subplots_adjust(
    bottom=0.12,
    wspace=0.35,
    hspace=0.35
)

plt.tight_layout()
plt.savefig("coral_boxplots.png", dpi=600, bbox_inches="tight")
plt.show()
```


    
![png](output_1_0.png)
    



```python
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np

# --------------------------
# LOAD DATA
# --------------------------
df = pd.read_csv("Functional Groups.csv")

# --------------------------
# CREATE LOCATION GROUPS
# --------------------------
df['Location'] = df['Site'].apply(
    lambda x: 'Papagayo' if 'Papagayo' in x else 'Samara'
)

# --------------------------
# FUNCTIONAL GROUPS (ORDERED)
# --------------------------
func_cols = [
    'Stony corals',
    'CCA',
    'Macroalgae',
    'Sea urchins',
    'Soft Coral',
    'Filteer feeders',
    'Other'
]

# --------------------------
# TITLE MAP
# --------------------------
title_map = {
    'Stony corals': 'Stony Corals',
    'CCA': 'CCA',
    'Macroalgae': 'Macroalgae',
    'Sea urchins': 'Sea Urchins',
    'Soft Coral': 'Soft Corals',
    'Filteer feeders': 'Filter feeders',
    'Other': 'Other'
}

# --------------------------
# SIGNIFICANCE STARS
# --------------------------
sig_map = {
    'Stony corals': '***',
    'CCA': '***',
    'Macroalgae': '***',
    'Sea urchins': '***',
    'Soft Coral': '**'
}

# --------------------------
# COLORS
# --------------------------
palette = {
    'Samara': '#E69F00',
    'Papagayo': '#0072B2'
}

# --------------------------
# FONT SETTINGS
# --------------------------
plt.rcParams.update({
    "font.family": "Times New Roman",
    "font.size": 26,
    "axes.titlesize": 32,
    "axes.labelsize": 28,
    "ytick.labelsize": 26,
    "legend.fontsize": 26
})

# --------------------------
# CREATE SUBPLOTS
# --------------------------
n_cols = 3
n_rows = int(np.ceil(len(func_cols) / n_cols))

fig, axes = plt.subplots(n_rows, n_cols, figsize=(18, 14))
axes = axes.flatten()

# --------------------------
# LOOP THROUGH GROUPS
# --------------------------
for i, col in enumerate(func_cols):

    ax = axes[i]

    plot_df = df[df[col] > 0].copy()

    if len(plot_df) == 0:
        ax.set_visible(False)
        continue

    y_max = plot_df[col].max() * 1.1

    sns.boxplot(
        data=plot_df,
        x='Location',
        y=col,
        hue='Location',
        dodge=False,
        palette=palette,
        width=0.6,
        linewidth=2.5,
        showfliers=True,
        saturation=1,
        ax=ax
    )

    # remove legend
    if ax.get_legend() is not None:
        ax.get_legend().remove()

    # remove axis labels
    ax.set_xlabel('')
    ax.set_ylabel('')

    # remove x-axis labels + ticks
    ax.set_xticklabels([])
    ax.tick_params(
        axis='x',
        which='both',
        bottom=False,
        top=False,
        length=0,
        labelbottom=False
    )

    # y-axis styling
    ax.tick_params(
        axis='y',
        which='major',
        length=8,
        width=2,
        labelsize=26
    )

    # title
    ax.set_title(title_map[col], fontsize=32, pad=15)

    # y-limit
    ax.set_ylim(0, y_max)

    # --------------------------
    # ASTERISKS INSIDE PLOT (TOP CENTER)
    # --------------------------
    stars = sig_map.get(col, '')
    if stars:
        ax.text(
            0.5, 0.95,
            stars,
            transform=ax.transAxes,
            ha='center',
            va='top',
            fontsize=34,
            fontweight='bold'
        )

    # full border
    for spine in ax.spines.values():
        spine.set_visible(True)
        spine.set_linewidth(2.5)

# hide empty panels
for j in range(len(func_cols), len(axes)):
    axes[j].set_visible(False)

plt.subplots_adjust(
    bottom=0.12,
    wspace=0.35,
    hspace=0.35
)

plt.tight_layout()
plt.savefig("functionalgroups_boxplots.png", dpi=600, bbox_inches="tight")
plt.show()
```


    
![png](output_2_0.png)
    



```python

```
