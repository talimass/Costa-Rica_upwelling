```python
from zipfile import ZipFile
import pandas as pd
import numpy as np

# Unzipping each CSV file from the zip file
def load_csvs_from_zip(zip_name, *, concat=True):
    zipped = ZipFile(zip_name, 'r')
    dfs = {}

    for name in zipped.namelist():
        if name.lower().endswith('.csv'):
            with zipped.open(name) as f:
                df = pd.read_csv(f)
                df['source_file'] = name  # Add source file name
                dfs[name] = df

    if concat:
        return pd.concat(dfs.values(), ignore_index=True)
    else:
        return dfs

# Example:
df_CTD = load_csvs_from_zip('CTD.zip')  # combined into one DataFrame
df_PRODSS = load_csvs_from_zip('ProDSS.zip')  # Now includes 'source_file' column


```


```python
# Takeing out all rows that dont have a vaule in Top_bottom collum
df_CTD = df_CTD[df_CTD['top_bottom'].notna()]
df_CTD
```




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
      <th>date</th>
      <th>station</th>
      <th>TEMP</th>
      <th>CNDC</th>
      <th>prSM</th>
      <th>PSAL</th>
      <th>DEPTH</th>
      <th>density</th>
      <th>flSP</th>
      <th>obs</th>
      <th>flag</th>
      <th>profile</th>
      <th>mld</th>
      <th>chla_ugL</th>
      <th>top_bottom</th>
      <th>mld_surface_subsurface</th>
      <th>source_file</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>240</th>
      <td>Feb 05 2025 00:35:51</td>
      <td>Samara S4 deeper site</td>
      <td>26.8971</td>
      <td>5.250927</td>
      <td>8.501</td>
      <td>33.1780</td>
      <td>8.436</td>
      <td>1021.4153</td>
      <td>0.47619</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>NaN</td>
      <td>4.7619</td>
      <td>bottom1m</td>
      <td>NaN</td>
      <td>2025_02_04_CR_S4_deep_cast 1_modified.csv</td>
    </tr>
    <tr>
      <th>241</th>
      <td>Feb 05 2025 00:35:51</td>
      <td>Samara S4 deeper site</td>
      <td>26.9014</td>
      <td>5.252280</td>
      <td>8.501</td>
      <td>33.1845</td>
      <td>8.436</td>
      <td>1021.4188</td>
      <td>0.48840</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>NaN</td>
      <td>4.8840</td>
      <td>bottom1m</td>
      <td>NaN</td>
      <td>2025_02_04_CR_S4_deep_cast 1_modified.csv</td>
    </tr>
    <tr>
      <th>242</th>
      <td>Feb 05 2025 00:35:51</td>
      <td>Samara S4 deeper site</td>
      <td>26.9250</td>
      <td>5.252577</td>
      <td>8.501</td>
      <td>33.1697</td>
      <td>8.436</td>
      <td>1021.4002</td>
      <td>0.52503</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>NaN</td>
      <td>5.2503</td>
      <td>bottom1m</td>
      <td>NaN</td>
      <td>2025_02_04_CR_S4_deep_cast 1_modified.csv</td>
    </tr>
    <tr>
      <th>243</th>
      <td>Feb 05 2025 00:35:51</td>
      <td>Samara S4 deeper site</td>
      <td>27.0009</td>
      <td>5.258457</td>
      <td>8.488</td>
      <td>33.1573</td>
      <td>8.422</td>
      <td>1021.3667</td>
      <td>0.50061</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>NaN</td>
      <td>5.0061</td>
      <td>bottom1m</td>
      <td>NaN</td>
      <td>2025_02_04_CR_S4_deep_cast 1_modified.csv</td>
    </tr>
    <tr>
      <th>244</th>
      <td>Feb 05 2025 00:35:51</td>
      <td>Samara S4 deeper site</td>
      <td>27.0803</td>
      <td>5.257394</td>
      <td>8.448</td>
      <td>33.0932</td>
      <td>8.383</td>
      <td>1021.2932</td>
      <td>0.48840</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>NaN</td>
      <td>4.8840</td>
      <td>bottom1m</td>
      <td>NaN</td>
      <td>2025_02_04_CR_S4_deep_cast 1_modified.csv</td>
    </tr>
    <tr>
      <th>...</th>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
    </tr>
    <tr>
      <th>3682</th>
      <td>Feb 08 2025 23:19:52</td>
      <td>Papagayo Site 5</td>
      <td>24.8216</td>
      <td>5.154136</td>
      <td>0.321</td>
      <td>34.0019</td>
      <td>0.318</td>
      <td>1022.6424</td>
      <td>0.50061</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>1.3</td>
      <td>5.0061</td>
      <td>top1m</td>
      <td>above_mld</td>
      <td>2025_02_08_CR_P5_cast 1_modified.csv</td>
    </tr>
    <tr>
      <th>3683</th>
      <td>Feb 08 2025 23:19:52</td>
      <td>Papagayo Site 5</td>
      <td>24.8216</td>
      <td>5.153835</td>
      <td>0.268</td>
      <td>33.9997</td>
      <td>0.266</td>
      <td>1022.6405</td>
      <td>0.46398</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>1.3</td>
      <td>4.6398</td>
      <td>top1m</td>
      <td>above_mld</td>
      <td>2025_02_08_CR_P5_cast 1_modified.csv</td>
    </tr>
    <tr>
      <th>3684</th>
      <td>Feb 08 2025 23:19:52</td>
      <td>Papagayo Site 5</td>
      <td>24.8231</td>
      <td>5.154740</td>
      <td>0.188</td>
      <td>34.0053</td>
      <td>0.187</td>
      <td>1022.6439</td>
      <td>0.45177</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>1.3</td>
      <td>4.5177</td>
      <td>top1m</td>
      <td>above_mld</td>
      <td>2025_02_08_CR_P5_cast 1_modified.csv</td>
    </tr>
    <tr>
      <th>3685</th>
      <td>Feb 08 2025 23:19:52</td>
      <td>Papagayo Site 5</td>
      <td>24.8231</td>
      <td>5.155494</td>
      <td>0.109</td>
      <td>34.0109</td>
      <td>0.108</td>
      <td>1022.6478</td>
      <td>0.46398</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>1.3</td>
      <td>4.6398</td>
      <td>top1m</td>
      <td>above_mld</td>
      <td>2025_02_08_CR_P5_cast 1_modified.csv</td>
    </tr>
    <tr>
      <th>3686</th>
      <td>Feb 08 2025 23:19:52</td>
      <td>Papagayo Site 5</td>
      <td>24.8327</td>
      <td>5.155343</td>
      <td>0.016</td>
      <td>34.0026</td>
      <td>0.016</td>
      <td>1022.6383</td>
      <td>0.00000</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>1.3</td>
      <td>0.0000</td>
      <td>top1m</td>
      <td>above_mld</td>
      <td>2025_02_08_CR_P5_cast 1_modified.csv</td>
    </tr>
  </tbody>
</table>
<p>322 rows × 17 columns</p>
</div>




```python
#CTF Filtering top and Bottom 
# Step 1: Filter out rows with 'nursery' in the Station column (case insensitive)
df_CTD_filtered = df_CTD[~df_CTD['station'].str.contains('nursery', case=False, na=False)]

# Step 2: Filter for rows with only 'top1m' or 'bottom1m' in top_bottom
df_CTD_filtered = df_CTD_filtered[df_CTD_filtered['top_bottom'].isin(['top1m', 'bottom1m'])]


# Step 3: Create four dataframes
# Samara
df_samara_top = df_CTD_filtered[df_CTD_filtered['station'].str.contains('samara', case=False) &
                                (df_CTD_filtered['top_bottom'] == 'top1m')]

df_samara_bottom = df_CTD_filtered[df_CTD_filtered['station'].str.contains('samara', case=False) &
                                   (df_CTD_filtered['top_bottom'] == 'bottom1m')]

# Papagayo
df_papagayo_top = df_CTD_filtered[df_CTD_filtered['station'].str.contains('papagayo', case=False) &
                                  (df_CTD_filtered['top_bottom'] == 'top1m')]

df_papagayo_bottom = df_CTD_filtered[df_CTD_filtered['station'].str.contains('papagayo', case=False) &
                                     (df_CTD_filtered['top_bottom'] == 'bottom1m')]

```


```python
df_CTD_filtered

```




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
      <th>date</th>
      <th>station</th>
      <th>TEMP</th>
      <th>CNDC</th>
      <th>prSM</th>
      <th>PSAL</th>
      <th>DEPTH</th>
      <th>density</th>
      <th>flSP</th>
      <th>obs</th>
      <th>flag</th>
      <th>profile</th>
      <th>mld</th>
      <th>chla_ugL</th>
      <th>top_bottom</th>
      <th>mld_surface_subsurface</th>
      <th>source_file</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>240</th>
      <td>Feb 05 2025 00:35:51</td>
      <td>Samara S4 deeper site</td>
      <td>26.8971</td>
      <td>5.250927</td>
      <td>8.501</td>
      <td>33.1780</td>
      <td>8.436</td>
      <td>1021.4153</td>
      <td>0.47619</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>NaN</td>
      <td>4.7619</td>
      <td>bottom1m</td>
      <td>NaN</td>
      <td>2025_02_04_CR_S4_deep_cast 1_modified.csv</td>
    </tr>
    <tr>
      <th>241</th>
      <td>Feb 05 2025 00:35:51</td>
      <td>Samara S4 deeper site</td>
      <td>26.9014</td>
      <td>5.252280</td>
      <td>8.501</td>
      <td>33.1845</td>
      <td>8.436</td>
      <td>1021.4188</td>
      <td>0.48840</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>NaN</td>
      <td>4.8840</td>
      <td>bottom1m</td>
      <td>NaN</td>
      <td>2025_02_04_CR_S4_deep_cast 1_modified.csv</td>
    </tr>
    <tr>
      <th>242</th>
      <td>Feb 05 2025 00:35:51</td>
      <td>Samara S4 deeper site</td>
      <td>26.9250</td>
      <td>5.252577</td>
      <td>8.501</td>
      <td>33.1697</td>
      <td>8.436</td>
      <td>1021.4002</td>
      <td>0.52503</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>NaN</td>
      <td>5.2503</td>
      <td>bottom1m</td>
      <td>NaN</td>
      <td>2025_02_04_CR_S4_deep_cast 1_modified.csv</td>
    </tr>
    <tr>
      <th>243</th>
      <td>Feb 05 2025 00:35:51</td>
      <td>Samara S4 deeper site</td>
      <td>27.0009</td>
      <td>5.258457</td>
      <td>8.488</td>
      <td>33.1573</td>
      <td>8.422</td>
      <td>1021.3667</td>
      <td>0.50061</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>NaN</td>
      <td>5.0061</td>
      <td>bottom1m</td>
      <td>NaN</td>
      <td>2025_02_04_CR_S4_deep_cast 1_modified.csv</td>
    </tr>
    <tr>
      <th>244</th>
      <td>Feb 05 2025 00:35:51</td>
      <td>Samara S4 deeper site</td>
      <td>27.0803</td>
      <td>5.257394</td>
      <td>8.448</td>
      <td>33.0932</td>
      <td>8.383</td>
      <td>1021.2932</td>
      <td>0.48840</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>NaN</td>
      <td>4.8840</td>
      <td>bottom1m</td>
      <td>NaN</td>
      <td>2025_02_04_CR_S4_deep_cast 1_modified.csv</td>
    </tr>
    <tr>
      <th>...</th>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
    </tr>
    <tr>
      <th>3682</th>
      <td>Feb 08 2025 23:19:52</td>
      <td>Papagayo Site 5</td>
      <td>24.8216</td>
      <td>5.154136</td>
      <td>0.321</td>
      <td>34.0019</td>
      <td>0.318</td>
      <td>1022.6424</td>
      <td>0.50061</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>1.3</td>
      <td>5.0061</td>
      <td>top1m</td>
      <td>above_mld</td>
      <td>2025_02_08_CR_P5_cast 1_modified.csv</td>
    </tr>
    <tr>
      <th>3683</th>
      <td>Feb 08 2025 23:19:52</td>
      <td>Papagayo Site 5</td>
      <td>24.8216</td>
      <td>5.153835</td>
      <td>0.268</td>
      <td>33.9997</td>
      <td>0.266</td>
      <td>1022.6405</td>
      <td>0.46398</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>1.3</td>
      <td>4.6398</td>
      <td>top1m</td>
      <td>above_mld</td>
      <td>2025_02_08_CR_P5_cast 1_modified.csv</td>
    </tr>
    <tr>
      <th>3684</th>
      <td>Feb 08 2025 23:19:52</td>
      <td>Papagayo Site 5</td>
      <td>24.8231</td>
      <td>5.154740</td>
      <td>0.188</td>
      <td>34.0053</td>
      <td>0.187</td>
      <td>1022.6439</td>
      <td>0.45177</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>1.3</td>
      <td>4.5177</td>
      <td>top1m</td>
      <td>above_mld</td>
      <td>2025_02_08_CR_P5_cast 1_modified.csv</td>
    </tr>
    <tr>
      <th>3685</th>
      <td>Feb 08 2025 23:19:52</td>
      <td>Papagayo Site 5</td>
      <td>24.8231</td>
      <td>5.155494</td>
      <td>0.109</td>
      <td>34.0109</td>
      <td>0.108</td>
      <td>1022.6478</td>
      <td>0.46398</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>1.3</td>
      <td>4.6398</td>
      <td>top1m</td>
      <td>above_mld</td>
      <td>2025_02_08_CR_P5_cast 1_modified.csv</td>
    </tr>
    <tr>
      <th>3686</th>
      <td>Feb 08 2025 23:19:52</td>
      <td>Papagayo Site 5</td>
      <td>24.8327</td>
      <td>5.155343</td>
      <td>0.016</td>
      <td>34.0026</td>
      <td>0.016</td>
      <td>1022.6383</td>
      <td>0.00000</td>
      <td>0.0</td>
      <td>0</td>
      <td>up</td>
      <td>1.3</td>
      <td>0.0000</td>
      <td>top1m</td>
      <td>above_mld</td>
      <td>2025_02_08_CR_P5_cast 1_modified.csv</td>
    </tr>
  </tbody>
</table>
<p>306 rows × 17 columns</p>
</div>




```python
#Clean ProDSS Data 

df_PRODSS = df_PRODSS[df_PRODSS['top_bottom'].notna()]
df_PRODSS
```




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
      <th>Date</th>
      <th>Time</th>
      <th>C</th>
      <th>mmHg</th>
      <th>DO %</th>
      <th>DO mg/L</th>
      <th>C-uS/cm</th>
      <th>SAL-PSU</th>
      <th>Sigma</th>
      <th>pH</th>
      <th>...</th>
      <th>PE ug/L</th>
      <th>PC ug/L</th>
      <th>DEP m</th>
      <th>VPos m</th>
      <th>DEP bar a</th>
      <th>DEP bar g</th>
      <th>ALT m</th>
      <th>profile</th>
      <th>top_bottom</th>
      <th>source_file</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>23</th>
      <td>2/3/25</td>
      <td>16:37:52</td>
      <td>29.1</td>
      <td>754.6</td>
      <td>132.5</td>
      <td>8.51</td>
      <td>53391</td>
      <td>32.29</td>
      <td>20.0</td>
      <td>8.04</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>2.856</td>
      <td>2.862</td>
      <td>0.286</td>
      <td>NaN</td>
      <td>17.0</td>
      <td>down</td>
      <td>bottom1m</td>
      <td>020325_ProDSS_S1_modified.csv</td>
    </tr>
    <tr>
      <th>24</th>
      <td>2/3/25</td>
      <td>16:37:53</td>
      <td>29.0</td>
      <td>754.7</td>
      <td>132.0</td>
      <td>8.49</td>
      <td>53335</td>
      <td>32.29</td>
      <td>20.0</td>
      <td>8.04</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>3.016</td>
      <td>2.946</td>
      <td>0.302</td>
      <td>NaN</td>
      <td>16.8</td>
      <td>down</td>
      <td>bottom1m</td>
      <td>020325_ProDSS_S1_modified.csv</td>
    </tr>
    <tr>
      <th>25</th>
      <td>2/3/25</td>
      <td>16:37:54</td>
      <td>29.0</td>
      <td>754.7</td>
      <td>131.7</td>
      <td>8.47</td>
      <td>53307</td>
      <td>32.29</td>
      <td>20.1</td>
      <td>8.04</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>3.255</td>
      <td>3.116</td>
      <td>0.326</td>
      <td>NaN</td>
      <td>16.9</td>
      <td>down</td>
      <td>bottom1m</td>
      <td>020325_ProDSS_S1_modified.csv</td>
    </tr>
    <tr>
      <th>26</th>
      <td>2/3/25</td>
      <td>16:37:55</td>
      <td>29.0</td>
      <td>754.5</td>
      <td>131.4</td>
      <td>8.46</td>
      <td>53286</td>
      <td>32.29</td>
      <td>20.1</td>
      <td>8.04</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>3.144</td>
      <td>3.165</td>
      <td>0.315</td>
      <td>NaN</td>
      <td>16.8</td>
      <td>down</td>
      <td>bottom1m</td>
      <td>020325_ProDSS_S1_modified.csv</td>
    </tr>
    <tr>
      <th>27</th>
      <td>2/3/25</td>
      <td>16:37:56</td>
      <td>29.0</td>
      <td>754.5</td>
      <td>131.1</td>
      <td>8.44</td>
      <td>53275</td>
      <td>32.29</td>
      <td>20.1</td>
      <td>8.04</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>3.216</td>
      <td>3.274</td>
      <td>0.322</td>
      <td>NaN</td>
      <td>16.8</td>
      <td>down</td>
      <td>bottom1m</td>
      <td>020325_ProDSS_S1_modified.csv</td>
    </tr>
    <tr>
      <th>...</th>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
    </tr>
    <tr>
      <th>2006</th>
      <td>2/3/25</td>
      <td>15:01:51</td>
      <td>28.8</td>
      <td>755.6</td>
      <td>112.4</td>
      <td>7.26</td>
      <td>53177</td>
      <td>32.35</td>
      <td>20.2</td>
      <td>8.03</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.313</td>
      <td>0.317</td>
      <td>0.031</td>
      <td>NaN</td>
      <td>10.6</td>
      <td>up</td>
      <td>top1m</td>
      <td>020325_ProDSS_Samara_coral nursery_modified.csv</td>
    </tr>
    <tr>
      <th>2007</th>
      <td>2/3/25</td>
      <td>15:01:53</td>
      <td>28.8</td>
      <td>755.6</td>
      <td>112.5</td>
      <td>7.26</td>
      <td>53195</td>
      <td>32.36</td>
      <td>20.2</td>
      <td>8.03</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.314</td>
      <td>0.312</td>
      <td>0.031</td>
      <td>NaN</td>
      <td>10.7</td>
      <td>up</td>
      <td>top1m</td>
      <td>020325_ProDSS_Samara_coral nursery_modified.csv</td>
    </tr>
    <tr>
      <th>2008</th>
      <td>2/3/25</td>
      <td>15:01:54</td>
      <td>28.8</td>
      <td>755.6</td>
      <td>112.6</td>
      <td>7.27</td>
      <td>53202</td>
      <td>32.36</td>
      <td>20.2</td>
      <td>8.03</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.313</td>
      <td>0.306</td>
      <td>0.031</td>
      <td>NaN</td>
      <td>10.8</td>
      <td>up</td>
      <td>top1m</td>
      <td>020325_ProDSS_Samara_coral nursery_modified.csv</td>
    </tr>
    <tr>
      <th>2009</th>
      <td>2/3/25</td>
      <td>15:01:55</td>
      <td>28.8</td>
      <td>755.6</td>
      <td>112.6</td>
      <td>7.27</td>
      <td>53203</td>
      <td>32.36</td>
      <td>20.2</td>
      <td>8.03</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.314</td>
      <td>0.319</td>
      <td>0.031</td>
      <td>NaN</td>
      <td>11.0</td>
      <td>up</td>
      <td>top1m</td>
      <td>020325_ProDSS_Samara_coral nursery_modified.csv</td>
    </tr>
    <tr>
      <th>2010</th>
      <td>2/3/25</td>
      <td>15:01:56</td>
      <td>28.8</td>
      <td>755.6</td>
      <td>112.6</td>
      <td>7.27</td>
      <td>53199</td>
      <td>32.35</td>
      <td>20.1</td>
      <td>8.03</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.315</td>
      <td>0.325</td>
      <td>0.032</td>
      <td>NaN</td>
      <td>11.3</td>
      <td>up</td>
      <td>top1m</td>
      <td>020325_ProDSS_Samara_coral nursery_modified.csv</td>
    </tr>
  </tbody>
</table>
<p>1034 rows × 27 columns</p>
</div>




```python
#Getting ride of the coral nursry for Pro DSS Data 
df_PRODSS = df_PRODSS[df_PRODSS['top_bottom'].notna()].copy()

df_PRODSS['Date'] = pd.to_datetime(df_PRODSS['Date']).dt.normalize()

papagayo_dates = pd.to_datetime(['2025-02-06','2025-02-07','2025-02-08'])
samara_dates = pd.to_datetime(['2025-02-02','2025-02-03','2025-02-04'])

df_PRODSS.loc[df_PRODSS['Date'].isin(papagayo_dates), 'station'] = 'Papagayo'
df_PRODSS.loc[df_PRODSS['Date'].isin(samara_dates), 'station'] = 'Samara'

# ⭐ CLEAN FILTER GOES HERE (IMPORTANT STEP)
df_PRODSS_clean = df_PRODSS[
    df_PRODSS['station'].notna() &
    df_PRODSS['top_bottom'].isin(['top1m','bottom1m']) &
    ~df_PRODSS['source_file'].str.contains('nursery|restoration', case=False, na=False)
].copy()
```

    /var/folders/b4/_jmnd3zs6vdcbnjlbry4mf340000gn/T/ipykernel_73287/3068508616.py:4: UserWarning: Could not infer format, so each element will be parsed individually, falling back to `dateutil`. To ensure parsing is consistent and as-expected, please specify a format.
      df_PRODSS['Date'] = pd.to_datetime(df_PRODSS['Date']).dt.normalize()



```python
#Check to make sure you getting ride of the coral nursry for Pro DSS Data 
df_PRODSS_clean['source_file'].value_counts()

```




    source_file
    020225_ProDSS_S2_modified.csv        156
    020825_ProDSS_P5_modified.csv        123
    020425_ProDSS_S4deep_modified.csv    119
    020725_ProDSS_P4_modified.csv         97
    020225_ProDSS_S1_modified.csv         66
    020725_ProDSS_P3_modified.csv         60
    020425_ProDSS_S1_modified.csv         59
    020625_ProDSS_P2_modified.csv         55
    020325_ProDSS_S3d_modified.csv        54
    020625_ProDSS_P1_modified.csv         45
    020825_ProDSS_P6_modified.csv         45
    020325_ProDSS_S1_modified.csv         36
    Name: count, dtype: int64




```python
#Filter ProDSS for bottom 1 m + Check 

df_PRODSS_samara_bottom = df_PRODSS_clean.query(
    "station == 'Samara' and top_bottom == 'bottom1m'"
)

df_PRODSS_papagayo_bottom = df_PRODSS_clean.query(
    "station == 'Papagayo' and top_bottom == 'bottom1m'"
)

df_PRODSS_samara_bottom['source_file'].value_counts()
```




    source_file
    020425_ProDSS_S4deep_modified.csv    99
    020225_ProDSS_S1_modified.csv        51
    020225_ProDSS_S2_modified.csv        49
    020425_ProDSS_S1_modified.csv        45
    020325_ProDSS_S1_modified.csv        36
    020325_ProDSS_S3d_modified.csv       34
    Name: count, dtype: int64




```python
#make CSV Files from the Data for each locations 

df_PRODSS_papagayo_bottom.to_csv('PRODSS_papagayo.csv', index=False)
df_PRODSS_samara_bottom.to_csv('PRODSS_samara.csv', index=False)
```


```python
print (df_papagayo_bottom)
print (df_samara_bottom)
print (df_PRODSS_papagayo_bottom)
print (df_PRODSS_samara_bottom)
```

                          date          station     TEMP      CNDC   prSM  \
    770   Feb 08 2025 23:20:15  Papagayo Site 6  24.7810  5.151178  9.260   
    771   Feb 08 2025 23:20:15  Papagayo Site 6  24.7802  5.151324  9.260   
    772   Feb 08 2025 23:20:15  Papagayo Site 6  24.7824  5.151168  9.246   
    773   Feb 08 2025 23:20:15  Papagayo Site 6  24.7817  5.151464  9.193   
    774   Feb 08 2025 23:20:15  Papagayo Site 6  24.7831  5.151308  9.167   
    ...                    ...              ...      ...       ...    ...   
    3583  Feb 08 2025 23:19:52  Papagayo Site 5  24.5317  5.130059  9.060   
    3584  Feb 08 2025 23:19:52  Papagayo Site 5  24.5317  5.130364  8.993   
    3585  Feb 08 2025 23:19:52  Papagayo Site 5  24.5340  5.130518  8.807   
    3586  Feb 08 2025 23:19:52  Papagayo Site 5  24.5340  5.130521  8.674   
    3587  Feb 08 2025 23:19:52  Papagayo Site 5  24.5347  5.130524  8.634   
    
             PSAL  DEPTH    density     flSP  obs  flag profile  mld  chla_ugL  \
    770   34.0079  9.189  1022.6978  0.61050  0.0     0      up  NaN    6.1050   
    771   34.0095  9.189  1022.6992  0.64713  0.0     0      up  NaN    6.4713   
    772   34.0067  9.175  1022.6964  0.59829  0.0     0      up  NaN    5.9829   
    773   34.0095  9.122  1022.6985  0.61050  0.0     0      up  NaN    6.1050   
    774   34.0072  9.096  1022.6962  0.58608  0.0     0      up  NaN    5.8608   
    ...       ...    ...        ...      ...  ...   ...     ...  ...       ...   
    3583  34.0395  8.990  1022.7956  0.62271  0.0     0      up  1.3    6.2271   
    3584  34.0417  8.924  1022.7970  0.75702  0.0     0      up  1.3    7.5702   
    3585  34.0413  8.739  1022.7952  0.68376  0.0     0      up  1.3    6.8376   
    3586  34.0413  8.607  1022.7946  0.59829  0.0     0      up  1.3    5.9829   
    3587  34.0408  8.568  1022.7938  0.64713  0.0     0      up  1.3    6.4713   
    
         top_bottom mld_surface_subsurface                           source_file  
    770    bottom1m                    NaN  2025_02_08_CR_P6_cast 1_modified.csv  
    771    bottom1m                    NaN  2025_02_08_CR_P6_cast 1_modified.csv  
    772    bottom1m                    NaN  2025_02_08_CR_P6_cast 1_modified.csv  
    773    bottom1m                    NaN  2025_02_08_CR_P6_cast 1_modified.csv  
    774    bottom1m                    NaN  2025_02_08_CR_P6_cast 1_modified.csv  
    ...         ...                    ...                                   ...  
    3583   bottom1m              below_mld  2025_02_08_CR_P5_cast 1_modified.csv  
    3584   bottom1m              below_mld  2025_02_08_CR_P5_cast 1_modified.csv  
    3585   bottom1m              below_mld  2025_02_08_CR_P5_cast 1_modified.csv  
    3586   bottom1m              below_mld  2025_02_08_CR_P5_cast 1_modified.csv  
    3587   bottom1m              below_mld  2025_02_08_CR_P5_cast 1_modified.csv  
    
    [100 rows x 17 columns]
                          date                station     TEMP      CNDC   prSM  \
    240   Feb 05 2025 00:35:51  Samara S4 deeper site  26.8971  5.250927  8.501   
    241   Feb 05 2025 00:35:51  Samara S4 deeper site  26.9014  5.252280  8.501   
    242   Feb 05 2025 00:35:51  Samara S4 deeper site  26.9250  5.252577  8.501   
    243   Feb 05 2025 00:35:51  Samara S4 deeper site  27.0009  5.258457  8.488   
    244   Feb 05 2025 00:35:51  Samara S4 deeper site  27.0803  5.257394  8.448   
    ...                    ...                    ...      ...       ...    ...   
    1861  Feb 02 2025 22:03:20    Costa Rica_Samara 1  29.2136  5.416526  1.434   
    1862  Feb 02 2025 22:03:20    Costa Rica_Samara 1  29.2150  5.416527  1.408   
    1863  Feb 02 2025 22:03:20    Costa Rica_Samara 1  29.2157  5.416528  1.408   
    1864  Feb 02 2025 22:03:20    Costa Rica_Samara 1  29.2171  5.416529  1.408   
    1865  Feb 02 2025 22:03:20    Costa Rica_Samara 1  29.2171  5.416531  1.341   
    
             PSAL  DEPTH    density     flSP  obs  flag profile  mld  chla_ugL  \
    240   33.1780  8.436  1021.4153  0.47619  0.0     0      up  NaN    4.7619   
    241   33.1845  8.436  1021.4188  0.48840  0.0     0      up  NaN    4.8840   
    242   33.1697  8.436  1021.4002  0.52503  0.0     0      up  NaN    5.2503   
    243   33.1573  8.422  1021.3667  0.50061  0.0     0      up  NaN    5.0061   
    244   33.0932  8.383  1021.2932  0.48840  0.0     0      up  NaN    4.8840   
    ...       ...    ...        ...      ...  ...   ...     ...  ...       ...   
    1861  32.7124  1.423  1020.2827  0.35409  0.0     0      up  NaN    3.5409   
    1862  32.7115  1.397  1020.2815  0.37851  0.0     0      up  NaN    3.7851   
    1863  32.7110  1.397  1020.2809  0.35409  0.0     0      up  NaN    3.5409   
    1864  32.7101  1.397  1020.2797  0.34188  0.0     0      up  NaN    3.4188   
    1865  32.7101  1.331  1020.2794  0.43956  0.0     0      up  NaN    4.3956   
    
         top_bottom mld_surface_subsurface  \
    240    bottom1m                    NaN   
    241    bottom1m                    NaN   
    242    bottom1m                    NaN   
    243    bottom1m                    NaN   
    244    bottom1m                    NaN   
    ...         ...                    ...   
    1861   bottom1m                    NaN   
    1862   bottom1m                    NaN   
    1863   bottom1m                    NaN   
    1864   bottom1m                    NaN   
    1865   bottom1m                    NaN   
    
                                        source_file  
    240   2025_02_04_CR_S4_deep_cast 1_modified.csv  
    241   2025_02_04_CR_S4_deep_cast 1_modified.csv  
    242   2025_02_04_CR_S4_deep_cast 1_modified.csv  
    243   2025_02_04_CR_S4_deep_cast 1_modified.csv  
    244   2025_02_04_CR_S4_deep_cast 1_modified.csv  
    ...                                         ...  
    1861       2025_02_01_CR_S1_cast 1_modified.csv  
    1862       2025_02_01_CR_S1_cast 1_modified.csv  
    1863       2025_02_01_CR_S1_cast 1_modified.csv  
    1864       2025_02_01_CR_S1_cast 1_modified.csv  
    1865       2025_02_01_CR_S1_cast 1_modified.csv  
    
    [72 rows x 17 columns]
               Date      Time     C   mmHg   DO %  DO mg/L  C-uS/cm  SAL-PSU  \
    123  2025-02-06  14:37:53  25.5  756.2  117.9     7.96    52366    34.12   
    124  2025-02-06  14:37:54  25.5  756.2  118.0     7.97    52362    34.12   
    125  2025-02-06  14:37:55  25.5  756.2  118.1     7.97    52357    34.12   
    126  2025-02-06  14:37:56  25.5  756.2  118.1     7.98    52350    34.12   
    127  2025-02-06  14:37:57  25.4  756.2  118.1     7.98    52343    34.13   
    ...         ...       ...   ...    ...    ...      ...      ...      ...   
    1831 2025-02-08  11:28:15  23.9  758.4   87.2     6.03    51204    34.42   
    1832 2025-02-08  11:28:16  23.9  758.3   87.2     6.03    51205    34.42   
    1833 2025-02-08  11:28:17  23.9  758.4   87.2     6.03    51205    34.42   
    1834 2025-02-08  11:28:18  24.0  758.4   87.3     6.04    51206    34.42   
    1835 2025-02-08  11:28:19  24.0  758.3   87.3     6.04    51206    34.42   
    
          Sigma    pH  ...  PC ug/L  DEP m  VPos m  DEP bar a  DEP bar g  ALT m  \
    123    22.6  8.02  ...      NaN  8.907   8.807      0.893        NaN    1.9   
    124    22.6  8.02  ...      NaN  8.946   8.970      0.897        NaN    1.5   
    125    22.6  8.02  ...      NaN  9.030   9.168      0.905        NaN    2.2   
    126    22.6  8.02  ...      NaN  9.120   9.236      0.915        NaN    2.2   
    127    22.6  8.02  ...      NaN  9.191   9.375      0.922        NaN    2.4   
    ...     ...   ...  ...      ...    ...     ...        ...        ...    ...   
    1831   23.3  7.91  ...      NaN  9.331   9.139      0.936        NaN   16.1   
    1832   23.3  7.91  ...      NaN  9.325   9.063      0.936        NaN   16.1   
    1833   23.3  7.91  ...      NaN  9.314   8.956      0.935        NaN   16.2   
    1834   23.3  7.91  ...      NaN  9.305   8.852      0.934        NaN   16.3   
    1835   23.3  7.91  ...      NaN  9.291   8.710      0.932        NaN   16.4   
    
          profile  top_bottom                    source_file   station  
    123      down    bottom1m  020625_ProDSS_P2_modified.csv  Papagayo  
    124      down    bottom1m  020625_ProDSS_P2_modified.csv  Papagayo  
    125      down    bottom1m  020625_ProDSS_P2_modified.csv  Papagayo  
    126      down    bottom1m  020625_ProDSS_P2_modified.csv  Papagayo  
    127      down    bottom1m  020625_ProDSS_P2_modified.csv  Papagayo  
    ...       ...         ...                            ...       ...  
    1831       up    bottom1m  020825_ProDSS_P5_modified.csv  Papagayo  
    1832       up    bottom1m  020825_ProDSS_P5_modified.csv  Papagayo  
    1833       up    bottom1m  020825_ProDSS_P5_modified.csv  Papagayo  
    1834       up    bottom1m  020825_ProDSS_P5_modified.csv  Papagayo  
    1835       up    bottom1m  020825_ProDSS_P5_modified.csv  Papagayo  
    
    [328 rows x 28 columns]
               Date      Time     C   mmHg   DO %  DO mg/L  C-uS/cm  SAL-PSU  \
    23   2025-02-03  16:37:52  29.1  754.6  132.5     8.51    53391    32.29   
    24   2025-02-03  16:37:53  29.0  754.7  132.0     8.49    53335    32.29   
    25   2025-02-03  16:37:54  29.0  754.7  131.7     8.47    53307    32.29   
    26   2025-02-03  16:37:55  29.0  754.5  131.4     8.46    53286    32.29   
    27   2025-02-03  16:37:56  29.0  754.5  131.1     8.44    53275    32.29   
    ...         ...       ...   ...    ...    ...      ...      ...      ...   
    1608 2025-02-02  14:08:56  28.7  755.0  124.3     8.03    53128    32.34   
    1609 2025-02-02  14:08:57  28.7  755.1  124.4     8.04    53128    32.34   
    1610 2025-02-02  14:08:58  28.7  755.0  124.4     8.04    53127    32.34   
    1611 2025-02-02  14:08:59  28.7  755.1  124.5     8.04    53127    32.34   
    1612 2025-02-02  14:09:00  28.7  755.1  124.5     8.05    53127    32.34   
    
          Sigma    pH  ...  PC ug/L  DEP m  VPos m  DEP bar a  DEP bar g  ALT m  \
    23     20.0  8.04  ...      NaN  2.856   2.862      0.286        NaN   17.0   
    24     20.0  8.04  ...      NaN  3.016   2.946      0.302        NaN   16.8   
    25     20.1  8.04  ...      NaN  3.255   3.116      0.326        NaN   16.9   
    26     20.1  8.04  ...      NaN  3.144   3.165      0.315        NaN   16.8   
    27     20.1  8.04  ...      NaN  3.216   3.274      0.322        NaN   16.8   
    ...     ...   ...  ...      ...    ...     ...        ...        ...    ...   
    1608   20.2  8.10  ...      NaN  2.552   2.486      0.255        NaN   12.7   
    1609   20.2  8.10  ...      NaN  2.554   2.481      0.255        NaN   12.6   
    1610   20.2  8.10  ...      NaN  2.554   2.431      0.256        NaN   12.5   
    1611   20.2  8.10  ...      NaN  2.546   2.133      0.255        NaN   12.3   
    1612   20.2  8.10  ...      NaN  2.535   1.933      0.254        NaN   12.0   
    
          profile  top_bottom                    source_file  station  
    23       down    bottom1m  020325_ProDSS_S1_modified.csv   Samara  
    24       down    bottom1m  020325_ProDSS_S1_modified.csv   Samara  
    25       down    bottom1m  020325_ProDSS_S1_modified.csv   Samara  
    26       down    bottom1m  020325_ProDSS_S1_modified.csv   Samara  
    27       down    bottom1m  020325_ProDSS_S1_modified.csv   Samara  
    ...       ...         ...                            ...      ...  
    1608       up    bottom1m  020225_ProDSS_S1_modified.csv   Samara  
    1609       up    bottom1m  020225_ProDSS_S1_modified.csv   Samara  
    1610       up    bottom1m  020225_ProDSS_S1_modified.csv   Samara  
    1611       up    bottom1m  020225_ProDSS_S1_modified.csv   Samara  
    1612       up    bottom1m  020225_ProDSS_S1_modified.csv   Samara  
    
    [314 rows x 28 columns]



```python
#Combinging the Prodss data frames from Samara and papagayo (no coral nursrty) + Combining the CTD Data Frames from Samara and papagayo 

df_CTD_combined = pd.concat(
    [df_papagayo_bottom, df_samara_bottom],
    ignore_index=True
)

df_PRODSS_combined = pd.concat(
    [df_PRODSS_papagayo_bottom, df_PRODSS_samara_bottom],
    ignore_index=True
)


df_CTD_combined['station_simple'] = df_CTD_combined['station'].str.extract(r'^(Samara|Papagayo)')
df_PRODSS_combined['station_simple'] = df_PRODSS_combined['station'].str.extract(r'^(Samara|Papagayo)')

```


```python
import matplotlib.pyplot as plt
import seaborn as sns
import matplotlib.patches as mpatches

# --------------------------
# GLOBAL FONT SETTINGS
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
# PLOTTING SETTINGS
# --------------------------
title_fontsize = 32
ylabel_fontsize = 28
ylabel_ticksize = 26

station_order = ['Papagayo', 'Samara']

station_colors = {
    'Samara': '#E69F00',   # orange
    'Papagayo': '#0072B2'  # blue
}

plot_vars = ['TEMP', 'pH', 'PSAL', 'chla_ugL']

# --------------------------
# FIGURE SETUP (2x2 GRID)
# --------------------------
fig, axes = plt.subplots(2, 2, figsize=(18, 14))
axes = axes.flatten()

# Store y-limits for post-processing
ylim_store = {}

# --------------------------
# MAIN PLOTTING LOOP
# --------------------------
for ax, var in zip(axes, plot_vars):

    # Select correct dataframe
    if var in ['TEMP', 'PSAL', 'chla_ugL']:
        df_plot = df_CTD_combined
    else:
        df_plot = df_PRODSS_combined

    sns.boxplot(
        data=df_plot,
        x='station_simple',
        y=var,
        hue='station_simple',
        order=station_order,
        hue_order=station_order,
        palette=station_colors,
        ax=ax,
        legend=False,
        saturation=1
    )

    ax.set_title(custom_titles.get(var, var), fontsize=title_fontsize)
    ax.set_xlabel('')
    ax.set_xticklabels([])

    ax.set_ylabel(
        custom_ylabels.get(var, var),
        fontsize=ylabel_fontsize,
        labelpad=15
    )

    ax.tick_params(axis='y', labelsize=ylabel_ticksize)

    # Store original y-limits BEFORE layout adjustments
    ylim_store[var] = ax.get_ylim()

# --------------------------
# ADD WHITE SPACE + STARS
# --------------------------
for ax, var in zip(axes, plot_vars):
    ymin, ymax = ylim_store[var]
    data_range = ymax - ymin
    headroom = data_range * 0.15

    ax.set_ylim(ymin, ymax + headroom)

    ax.text(
        0.5,
        ymax + headroom * 0.3,
        '***',
        ha='center',
        va='center',
        fontsize=30,
        fontweight='bold'
    )

# --------------------------
# LEGEND
# --------------------------
sam_patch = mpatches.Patch(color='#E69F00', label='Samara (LU)')
pap_patch = mpatches.Patch(color='#0072B2', label='Papagayo (HU)')

fig.legend(
    handles=[sam_patch, pap_patch],
    loc='lower center',
    ncol=2,
    fontsize=26,
    title='Location',
    title_fontsize=28,
    frameon=True,
    borderpad=1.0,
    handlelength=3.0,
    handleheight=2.5,
    bbox_to_anchor=(0.5, 0.02)
)

# Leave space for legend
plt.subplots_adjust(bottom=0.18)
plt.subplots_adjust(wspace=0.35, hspace=0.30)

# --------------------------
# SAVE & SHOW
# --------------------------
plt.savefig("ENVFIG_poster.png", dpi=600, bbox_inches='tight')
plt.show()
```


    
![png](output_11_0.png)
    



```python
# Double check to make sure we are ony using data points from bottom 1 m 
assert all(df_papagayo_bottom['top_bottom'] == 'bottom1m')
assert all(df_samara_bottom['top_bottom'] == 'bottom1m')
```


```python
import pandas as pd
import scipy.stats as stats

# --------------------------
# VARIABLES
# --------------------------
ctd_vars = ["TEMP", "PSAL", "chla_ugL"]
prodss_vars = ["pH"]

results = []

# --------------------------
# USE COMBINED DATASETS
# --------------------------
datasets = {
    "CTD": df_CTD_combined.copy(),
    "ProDSS": df_PRODSS_combined.copy()
}

# ensure station_simple exists (safe fix)
for name in datasets:
    datasets[name]["station_simple"] = datasets[name]["station_simple"].astype(str)

# --------------------------
# FUNCTION FOR TESTING
# --------------------------
def run_tests(df, vars_list, dataset_name):

    for var in vars_list:

        group1 = df[df["station_simple"] == "Papagayo"][var].dropna()
        group2 = df[df["station_simple"] == "Samara"][var].dropna()

        # skip if too small
        if len(group1) < 3 or len(group2) < 3:
            continue

        # --------------------------
        # NORMALITY
        # --------------------------
        shapiro1 = stats.shapiro(group1)
        shapiro2 = stats.shapiro(group2)

        normal = (shapiro1.pvalue > 0.05) and (shapiro2.pvalue > 0.05)

        # --------------------------
        # TEST SELECTION
        # --------------------------
        if normal:
            test_name = "t-test"
            stat, pval = stats.ttest_ind(group1, group2, equal_var=False)
        else:
            test_name = "Mann-Whitney U"
            stat, pval = stats.mannwhitneyu(group1, group2, alternative="two-sided")

        results.append({
            "Dataset": dataset_name,
            "Variable": var,
            "Test": test_name,
            "Statistic": stat,
            "p-value": pval,
            "n_Papagayo": len(group1),
            "n_Samara": len(group2),
            "Shapiro_Papagayo": shapiro1.pvalue,
            "Shapiro_Samara": shapiro2.pvalue
        })

# --------------------------
# RUN BOTH DATASETS
# --------------------------
run_tests(datasets["CTD"], ctd_vars, "CTD")
run_tests(datasets["ProDSS"], prodss_vars, "ProDSS")

# --------------------------
# OUTPUT
# --------------------------
results_df = pd.DataFrame(results)
print(results_df)
```

      Dataset  Variable            Test  Statistic       p-value  n_Papagayo  \
    0     CTD      TEMP  Mann-Whitney U        0.0  1.566001e-18         100   
    1     CTD      PSAL  Mann-Whitney U     3500.0  1.569288e-18         100   
    2     CTD  chla_ugL  Mann-Whitney U     3490.5  2.371352e-18         100   
    3  ProDSS        pH  Mann-Whitney U    14671.0  4.080665e-56         328   
    
       n_Samara  Shapiro_Papagayo  Shapiro_Samara  
    0        35      2.268817e-07    7.003682e-07  
    1        35      1.870471e-08    2.970272e-06  
    2        35      5.327460e-06    4.143227e-05  
    3       314      2.340539e-20    5.696584e-11  



```python
# Checking for varibility within locations: Papagayo 

import pandas as pd
import scipy.stats as stats
import scikit_posthocs as sp

# --------------------------
# VARIABLES
# --------------------------
ctd_vars = ["TEMP", "PSAL", "chla_ugL"]
prodss_vars = ["pH"]

results = []
dunn_results = {}

# ======================================================
# ----------- CTD PAPAGAYO ------------------------------
# ======================================================
df_papagayo_ctd = df_papagayo_bottom.copy()

sites_ctd = df_papagayo_ctd['source_file'].dropna().unique()
print("CTD Papagayo casts:", sites_ctd)

for var in ctd_vars:

    df_var = df_papagayo_ctd[['source_file', var]].dropna()

    groups = [
        df_var[df_var['source_file'] == s][var]
        for s in sites_ctd
    ]
    groups = [g for g in groups if len(g) > 2]

    # --------------------------
    # NORMALITY
    # --------------------------
    shapiro_pvals = [stats.shapiro(g).pvalue for g in groups if len(g) >= 3]
    normal = all(p > 0.05 for p in shapiro_pvals)

    # --------------------------
    # MAIN TEST
    # --------------------------
    if normal:
        test_name = "One-way ANOVA"
        stat, pval = stats.f_oneway(*groups)
    else:
        test_name = "Kruskal-Wallis"
        stat, pval = stats.kruskal(*groups)

    # --------------------------
    # DUNN'S TEST
    # --------------------------
    if test_name == "Kruskal-Wallis" and pval < 0.05:
        dunn = sp.posthoc_dunn(
            df_var,
            val_col=var,
            group_col='source_file',
            p_adjust='bonferroni'
        )
        dunn_results[f"CTD_Papagayo_{var}"] = dunn

    results.append({
        "Dataset": "CTD",
        "Location": "Papagayo",
        "Variable": var,
        "Test": test_name,
        "Statistic": stat,
        "p-value": pval,
        "n_groups": len(groups)
    })


# ======================================================
# ----------- PRODSS PAPAGAYO ---------------------------
# ======================================================
df_papagayo_prodss = df_PRODSS_papagayo_bottom.copy()

sites_prodss = df_papagayo_prodss['source_file'].dropna().unique()
print("\nProDSS Papagayo casts:", sites_prodss)

for var in prodss_vars:

    df_var = df_papagayo_prodss[['source_file', var]].dropna()

    groups = [
        df_var[df_var['source_file'] == s][var]
        for s in sites_prodss
    ]
    groups = [g for g in groups if len(g) > 2]

    # Normality
    shapiro_pvals = [stats.shapiro(g).pvalue for g in groups if len(g) >= 3]
    normal = all(p > 0.05 for p in shapiro_pvals)

    # Main test
    if normal:
        test_name = "One-way ANOVA"
        stat, pval = stats.f_oneway(*groups)
    else:
        test_name = "Kruskal-Wallis"
        stat, pval = stats.kruskal(*groups)

    # Dunn's test
    if test_name == "Kruskal-Wallis" and pval < 0.05:
        dunn = sp.posthoc_dunn(
            df_var,
            val_col=var,
            group_col='source_file',
            p_adjust='bonferroni'
        )
        dunn_results[f"ProDSS_Papagayo_{var}"] = dunn

    results.append({
        "Dataset": "ProDSS",
        "Location": "Papagayo",
        "Variable": var,
        "Test": test_name,
        "Statistic": stat,
        "p-value": pval,
        "n_groups": len(groups)
    })


# ======================================================
# OUTPUT
# ======================================================
results_df = pd.DataFrame(results)

print("\nMAIN RESULTS (Papagayo):")
print(results_df)

print("\nDUNN POST-HOC RESULTS (Papagayo):")
for key, val in dunn_results.items():
    print(f"\n{key}")
    print(val)
```

    CTD Papagayo casts: ['2025_02_08_CR_P6_cast 1_modified.csv'
     '2025_02_06_CR_P2_cast 1_modified.csv'
     '2025_02_06_CR_P1_cast 1_modified.csv'
     '2025_02_07_CR_P4_cast 1_modified.csv'
     '2025_02_07_CR_P3_cast 1_modified.csv'
     '2025_02_08_CR_P5_cast 1_modified.csv']
    
    ProDSS Papagayo casts: ['020625_ProDSS_P2_modified.csv' '020625_ProDSS_P1_modified.csv'
     '020825_ProDSS_P6_modified.csv' '020725_ProDSS_P3_modified.csv'
     '020725_ProDSS_P4_modified.csv' '020825_ProDSS_P5_modified.csv']
    
    MAIN RESULTS (Papagayo):
      Dataset  Location  Variable            Test   Statistic       p-value  \
    0     CTD  Papagayo      TEMP  Kruskal-Wallis   95.540756  4.594672e-19   
    1     CTD  Papagayo      PSAL  Kruskal-Wallis   95.528713  4.621565e-19   
    2     CTD  Papagayo  chla_ugL  Kruskal-Wallis   90.297122  5.819773e-18   
    3  ProDSS  Papagayo        pH  Kruskal-Wallis  321.872851  1.979635e-67   
    
       n_groups  
    0         6  
    1         6  
    2         6  
    3         6  
    
    DUNN POST-HOC RESULTS (Papagayo):
    
    CTD_Papagayo_TEMP
                                          2025_02_06_CR_P1_cast 1_modified.csv  \
    2025_02_06_CR_P1_cast 1_modified.csv                          1.000000e+00   
    2025_02_06_CR_P2_cast 1_modified.csv                          1.000000e+00   
    2025_02_07_CR_P3_cast 1_modified.csv                          3.222193e-02   
    2025_02_07_CR_P4_cast 1_modified.csv                          1.905331e-01   
    2025_02_08_CR_P5_cast 1_modified.csv                          7.055283e-10   
    2025_02_08_CR_P6_cast 1_modified.csv                          5.111880e-05   
    
                                          2025_02_06_CR_P2_cast 1_modified.csv  \
    2025_02_06_CR_P1_cast 1_modified.csv                              1.000000   
    2025_02_06_CR_P2_cast 1_modified.csv                              1.000000   
    2025_02_07_CR_P3_cast 1_modified.csv                              1.000000   
    2025_02_07_CR_P4_cast 1_modified.csv                              0.024553   
    2025_02_08_CR_P5_cast 1_modified.csv                              0.007037   
    2025_02_08_CR_P6_cast 1_modified.csv                              0.455195   
    
                                          2025_02_07_CR_P3_cast 1_modified.csv  \
    2025_02_06_CR_P1_cast 1_modified.csv                              0.032222   
    2025_02_06_CR_P2_cast 1_modified.csv                              1.000000   
    2025_02_07_CR_P3_cast 1_modified.csv                              1.000000   
    2025_02_07_CR_P4_cast 1_modified.csv                              0.000004   
    2025_02_08_CR_P5_cast 1_modified.csv                              0.027333   
    2025_02_08_CR_P6_cast 1_modified.csv                              1.000000   
    
                                          2025_02_07_CR_P4_cast 1_modified.csv  \
    2025_02_06_CR_P1_cast 1_modified.csv                          1.905331e-01   
    2025_02_06_CR_P2_cast 1_modified.csv                          2.455331e-02   
    2025_02_07_CR_P3_cast 1_modified.csv                          4.323386e-06   
    2025_02_07_CR_P4_cast 1_modified.csv                          1.000000e+00   
    2025_02_08_CR_P5_cast 1_modified.csv                          3.751006e-16   
    2025_02_08_CR_P6_cast 1_modified.csv                          6.700435e-10   
    
                                          2025_02_08_CR_P5_cast 1_modified.csv  \
    2025_02_06_CR_P1_cast 1_modified.csv                          7.055283e-10   
    2025_02_06_CR_P2_cast 1_modified.csv                          7.036573e-03   
    2025_02_07_CR_P3_cast 1_modified.csv                          2.733322e-02   
    2025_02_07_CR_P4_cast 1_modified.csv                          3.751006e-16   
    2025_02_08_CR_P5_cast 1_modified.csv                          1.000000e+00   
    2025_02_08_CR_P6_cast 1_modified.csv                          1.000000e+00   
    
                                          2025_02_08_CR_P6_cast 1_modified.csv  
    2025_02_06_CR_P1_cast 1_modified.csv                          5.111880e-05  
    2025_02_06_CR_P2_cast 1_modified.csv                          4.551952e-01  
    2025_02_07_CR_P3_cast 1_modified.csv                          1.000000e+00  
    2025_02_07_CR_P4_cast 1_modified.csv                          6.700435e-10  
    2025_02_08_CR_P5_cast 1_modified.csv                          1.000000e+00  
    2025_02_08_CR_P6_cast 1_modified.csv                          1.000000e+00  
    
    CTD_Papagayo_PSAL
                                          2025_02_06_CR_P1_cast 1_modified.csv  \
    2025_02_06_CR_P1_cast 1_modified.csv                          1.000000e+00   
    2025_02_06_CR_P2_cast 1_modified.csv                          3.883138e-02   
    2025_02_07_CR_P3_cast 1_modified.csv                          4.716257e-06   
    2025_02_07_CR_P4_cast 1_modified.csv                          1.906174e-01   
    2025_02_08_CR_P5_cast 1_modified.csv                          6.882916e-17   
    2025_02_08_CR_P6_cast 1_modified.csv                          4.403720e-10   
    
                                          2025_02_06_CR_P2_cast 1_modified.csv  \
    2025_02_06_CR_P1_cast 1_modified.csv                              0.038831   
    2025_02_06_CR_P2_cast 1_modified.csv                              1.000000   
    2025_02_07_CR_P3_cast 1_modified.csv                              1.000000   
    2025_02_07_CR_P4_cast 1_modified.csv                              1.000000   
    2025_02_08_CR_P5_cast 1_modified.csv                              0.007042   
    2025_02_08_CR_P6_cast 1_modified.csv                              0.455352   
    
                                          2025_02_07_CR_P3_cast 1_modified.csv  \
    2025_02_06_CR_P1_cast 1_modified.csv                              0.000005   
    2025_02_06_CR_P2_cast 1_modified.csv                              1.000000   
    2025_02_07_CR_P3_cast 1_modified.csv                              1.000000   
    2025_02_07_CR_P4_cast 1_modified.csv                              0.143658   
    2025_02_08_CR_P5_cast 1_modified.csv                              0.027351   
    2025_02_08_CR_P6_cast 1_modified.csv                              1.000000   
    
                                          2025_02_07_CR_P4_cast 1_modified.csv  \
    2025_02_06_CR_P1_cast 1_modified.csv                          1.906174e-01   
    2025_02_06_CR_P2_cast 1_modified.csv                          1.000000e+00   
    2025_02_07_CR_P3_cast 1_modified.csv                          1.436583e-01   
    2025_02_07_CR_P4_cast 1_modified.csv                          1.000000e+00   
    2025_02_08_CR_P5_cast 1_modified.csv                          5.942570e-08   
    2025_02_08_CR_P6_cast 1_modified.csv                          6.412251e-04   
    
                                          2025_02_08_CR_P5_cast 1_modified.csv  \
    2025_02_06_CR_P1_cast 1_modified.csv                          6.882916e-17   
    2025_02_06_CR_P2_cast 1_modified.csv                          7.042391e-03   
    2025_02_07_CR_P3_cast 1_modified.csv                          2.735144e-02   
    2025_02_07_CR_P4_cast 1_modified.csv                          5.942570e-08   
    2025_02_08_CR_P5_cast 1_modified.csv                          1.000000e+00   
    2025_02_08_CR_P6_cast 1_modified.csv                          1.000000e+00   
    
                                          2025_02_08_CR_P6_cast 1_modified.csv  
    2025_02_06_CR_P1_cast 1_modified.csv                          4.403720e-10  
    2025_02_06_CR_P2_cast 1_modified.csv                          4.553519e-01  
    2025_02_07_CR_P3_cast 1_modified.csv                          1.000000e+00  
    2025_02_07_CR_P4_cast 1_modified.csv                          6.412251e-04  
    2025_02_08_CR_P5_cast 1_modified.csv                          1.000000e+00  
    2025_02_08_CR_P6_cast 1_modified.csv                          1.000000e+00  
    
    CTD_Papagayo_chla_ugL
                                          2025_02_06_CR_P1_cast 1_modified.csv  \
    2025_02_06_CR_P1_cast 1_modified.csv                          1.000000e+00   
    2025_02_06_CR_P2_cast 1_modified.csv                          1.000000e+00   
    2025_02_07_CR_P3_cast 1_modified.csv                          4.330545e-04   
    2025_02_07_CR_P4_cast 1_modified.csv                          1.089760e-01   
    2025_02_08_CR_P5_cast 1_modified.csv                          3.004893e-11   
    2025_02_08_CR_P6_cast 1_modified.csv                          1.158661e-10   
    
                                          2025_02_06_CR_P2_cast 1_modified.csv  \
    2025_02_06_CR_P1_cast 1_modified.csv                          1.000000e+00   
    2025_02_06_CR_P2_cast 1_modified.csv                          1.000000e+00   
    2025_02_07_CR_P3_cast 1_modified.csv                          1.072515e-03   
    2025_02_07_CR_P4_cast 1_modified.csv                          6.731697e-02   
    2025_02_08_CR_P5_cast 1_modified.csv                          2.235661e-08   
    2025_02_08_CR_P6_cast 1_modified.csv                          3.456271e-08   
    
                                          2025_02_07_CR_P3_cast 1_modified.csv  \
    2025_02_06_CR_P1_cast 1_modified.csv                              0.000433   
    2025_02_06_CR_P2_cast 1_modified.csv                              0.001073   
    2025_02_07_CR_P3_cast 1_modified.csv                              1.000000   
    2025_02_07_CR_P4_cast 1_modified.csv                              1.000000   
    2025_02_08_CR_P5_cast 1_modified.csv                              0.183950   
    2025_02_08_CR_P6_cast 1_modified.csv                              0.189758   
    
                                          2025_02_07_CR_P4_cast 1_modified.csv  \
    2025_02_06_CR_P1_cast 1_modified.csv                              0.108976   
    2025_02_06_CR_P2_cast 1_modified.csv                              0.067317   
    2025_02_07_CR_P3_cast 1_modified.csv                              1.000000   
    2025_02_07_CR_P4_cast 1_modified.csv                              1.000000   
    2025_02_08_CR_P5_cast 1_modified.csv                              0.000444   
    2025_02_08_CR_P6_cast 1_modified.csv                              0.000605   
    
                                          2025_02_08_CR_P5_cast 1_modified.csv  \
    2025_02_06_CR_P1_cast 1_modified.csv                          3.004893e-11   
    2025_02_06_CR_P2_cast 1_modified.csv                          2.235661e-08   
    2025_02_07_CR_P3_cast 1_modified.csv                          1.839503e-01   
    2025_02_07_CR_P4_cast 1_modified.csv                          4.442681e-04   
    2025_02_08_CR_P5_cast 1_modified.csv                          1.000000e+00   
    2025_02_08_CR_P6_cast 1_modified.csv                          1.000000e+00   
    
                                          2025_02_08_CR_P6_cast 1_modified.csv  
    2025_02_06_CR_P1_cast 1_modified.csv                          1.158661e-10  
    2025_02_06_CR_P2_cast 1_modified.csv                          3.456271e-08  
    2025_02_07_CR_P3_cast 1_modified.csv                          1.897578e-01  
    2025_02_07_CR_P4_cast 1_modified.csv                          6.047388e-04  
    2025_02_08_CR_P5_cast 1_modified.csv                          1.000000e+00  
    2025_02_08_CR_P6_cast 1_modified.csv                          1.000000e+00  
    
    ProDSS_Papagayo_pH
                                   020625_ProDSS_P1_modified.csv  \
    020625_ProDSS_P1_modified.csv                   1.000000e+00   
    020625_ProDSS_P2_modified.csv                   1.000000e+00   
    020725_ProDSS_P3_modified.csv                   6.948728e-12   
    020725_ProDSS_P4_modified.csv                   8.015020e-06   
    020825_ProDSS_P5_modified.csv                   3.189147e-38   
    020825_ProDSS_P6_modified.csv                   5.507635e-25   
    
                                   020625_ProDSS_P2_modified.csv  \
    020625_ProDSS_P1_modified.csv                   1.000000e+00   
    020625_ProDSS_P2_modified.csv                   1.000000e+00   
    020725_ProDSS_P3_modified.csv                   3.561674e-08   
    020725_ProDSS_P4_modified.csv                   1.059011e-02   
    020825_ProDSS_P5_modified.csv                   4.458074e-35   
    020825_ProDSS_P6_modified.csv                   7.908829e-21   
    
                                   020725_ProDSS_P3_modified.csv  \
    020625_ProDSS_P1_modified.csv                   6.948728e-12   
    020625_ProDSS_P2_modified.csv                   3.561674e-08   
    020725_ProDSS_P3_modified.csv                   1.000000e+00   
    020725_ProDSS_P4_modified.csv                   1.041863e-02   
    020825_ProDSS_P5_modified.csv                   2.480765e-06   
    020825_ProDSS_P6_modified.csv                   7.498935e-04   
    
                                   020725_ProDSS_P4_modified.csv  \
    020625_ProDSS_P1_modified.csv                   8.015020e-06   
    020625_ProDSS_P2_modified.csv                   1.059011e-02   
    020725_ProDSS_P3_modified.csv                   1.041863e-02   
    020725_ProDSS_P4_modified.csv                   1.000000e+00   
    020825_ProDSS_P5_modified.csv                   6.264738e-26   
    020825_ProDSS_P6_modified.csv                   3.781882e-13   
    
                                   020825_ProDSS_P5_modified.csv  \
    020625_ProDSS_P1_modified.csv                   3.189147e-38   
    020625_ProDSS_P2_modified.csv                   4.458074e-35   
    020725_ProDSS_P3_modified.csv                   2.480765e-06   
    020725_ProDSS_P4_modified.csv                   6.264738e-26   
    020825_ProDSS_P5_modified.csv                   1.000000e+00   
    020825_ProDSS_P6_modified.csv                   1.000000e+00   
    
                                   020825_ProDSS_P6_modified.csv  
    020625_ProDSS_P1_modified.csv                   5.507635e-25  
    020625_ProDSS_P2_modified.csv                   7.908829e-21  
    020725_ProDSS_P3_modified.csv                   7.498935e-04  
    020725_ProDSS_P4_modified.csv                   3.781882e-13  
    020825_ProDSS_P5_modified.csv                   1.000000e+00  
    020825_ProDSS_P6_modified.csv                   1.000000e+00  


    /Users/kaylacayemitte/miniconda3/envs/CostaRica2025_env/lib/python3.11/site-packages/scipy/stats/_axis_nan_policy.py:586: UserWarning: scipy.stats.shapiro: Input data has range zero. The results may not be accurate.
      res = hypotest_fun_out(*samples, **kwds)



```python
# Checking for varibility within locations: Samara 


import pandas as pd
import scipy.stats as stats
import scikit_posthocs as sp

# --------------------------
# VARIABLES
# --------------------------
ctd_vars = ["TEMP", "PSAL", "chla_ugL"]
prodss_vars = ["pH"]

results = []
dunn_results = {}

# ======================================================
# ----------- CTD SAMARA ONLY ---------------------------
# ======================================================
df_samara_ctd = df_samara_bottom.copy()

sites_ctd = df_samara_ctd['source_file'].dropna().unique()
print("CTD Samara casts:", sites_ctd)

for var in ctd_vars:

    df_var = df_samara_ctd[['source_file', var]].dropna()

    groups = [
        df_var[df_var['source_file'] == s][var]
        for s in sites_ctd
    ]
    groups = [g for g in groups if len(g) > 2]

    # Normality
    shapiro_pvals = [stats.shapiro(g).pvalue for g in groups if len(g) >= 3]
    normal = all(p > 0.05 for p in shapiro_pvals)

    # Main test
    if normal:
        test_name = "One-way ANOVA"
        stat, pval = stats.f_oneway(*groups)
    else:
        test_name = "Kruskal-Wallis"
        stat, pval = stats.kruskal(*groups)

    # Dunn's test (ONLY if needed)
    if test_name == "Kruskal-Wallis" and pval < 0.05:
        dunn = sp.posthoc_dunn(
            df_var,
            val_col=var,
            group_col='source_file',
            p_adjust='bonferroni'
        )
        dunn_results[f"CTD_Samara_{var}"] = dunn

    results.append({
        "Dataset": "CTD",
        "Location": "Samara",
        "Variable": var,
        "Test": test_name,
        "Statistic": stat,
        "p-value": pval,
        "n_groups": len(groups)
    })


# ======================================================
# ----------- PRODSS SAMARA ONLY ------------------------
# ======================================================
df_samara_prodss = df_PRODSS_samara_bottom.copy()

sites_prodss = df_samara_prodss['source_file'].dropna().unique()
print("\nProDSS Samara casts:", sites_prodss)

for var in prodss_vars:

    df_var = df_samara_prodss[['source_file', var]].dropna()

    groups = [
        df_var[df_var['source_file'] == s][var]
        for s in sites_prodss
    ]
    groups = [g for g in groups if len(g) > 2]

    shapiro_pvals = [stats.shapiro(g).pvalue for g in groups if len(g) >= 3]
    normal = all(p > 0.05 for p in shapiro_pvals)

    if normal:
        test_name = "One-way ANOVA"
        stat, pval = stats.f_oneway(*groups)
    else:
        test_name = "Kruskal-Wallis"
        stat, pval = stats.kruskal(*groups)

    # Dunn's test
    if test_name == "Kruskal-Wallis" and pval < 0.05:
        dunn = sp.posthoc_dunn(
            df_var,
            val_col=var,
            group_col='source_file',
            p_adjust='bonferroni'
        )
        dunn_results[f"ProDSS_Samara_{var}"] = dunn

    results.append({
        "Dataset": "ProDSS",
        "Location": "Samara",
        "Variable": var,
        "Test": test_name,
        "Statistic": stat,
        "p-value": pval,
        "n_groups": len(groups)
    })


# ======================================================
# OUTPUT
# ======================================================
results_df = pd.DataFrame(results)

print("\nMAIN RESULTS:")
print(results_df)

print("\nDUNN POST-HOC RESULTS:")
for key, val in dunn_results.items():
    print(f"\n{key}")
    print(val)
```

    CTD Samara casts: ['2025_02_04_CR_S4_deep_cast 1_modified.csv'
     '2025_02_03_CR_S2_cast 3_modified.csv'
     '2025_02_03_CR_S3d_cast 1_modified.csv'
     '2025_02_01_CR_S1_cast 1_modified.csv']
    
    ProDSS Samara casts: ['020325_ProDSS_S1_modified.csv' '020425_ProDSS_S4deep_modified.csv'
     '020325_ProDSS_S3d_modified.csv' '020225_ProDSS_S2_modified.csv'
     '020425_ProDSS_S1_modified.csv' '020225_ProDSS_S1_modified.csv']
    
    MAIN RESULTS:
      Dataset Location  Variable            Test    Statistic       p-value  \
    0     CTD   Samara      TEMP  Kruskal-Wallis    59.690013  6.846530e-13   
    1     CTD   Samara      PSAL   One-way ANOVA  3129.145741  8.985533e-73   
    2     CTD   Samara  chla_ugL  Kruskal-Wallis    59.295180  8.314045e-13   
    3  ProDSS   Samara        pH  Kruskal-Wallis   293.686230  2.279913e-61   
    
       n_groups  
    0         4  
    1         4  
    2         4  
    3         6  
    
    DUNN POST-HOC RESULTS:
    
    CTD_Samara_TEMP
                                               2025_02_01_CR_S1_cast 1_modified.csv  \
    2025_02_01_CR_S1_cast 1_modified.csv                               1.000000e+00   
    2025_02_03_CR_S2_cast 3_modified.csv                               1.862304e-02   
    2025_02_03_CR_S3d_cast 1_modified.csv                              1.174078e-01   
    2025_02_04_CR_S4_deep_cast 1_modified.csv                          1.705872e-08   
    
                                               2025_02_03_CR_S2_cast 3_modified.csv  \
    2025_02_01_CR_S1_cast 1_modified.csv                               1.862304e-02   
    2025_02_03_CR_S2_cast 3_modified.csv                               1.000000e+00   
    2025_02_03_CR_S3d_cast 1_modified.csv                              3.277884e-04   
    2025_02_04_CR_S4_deep_cast 1_modified.csv                          4.563128e-11   
    
                                               2025_02_03_CR_S3d_cast 1_modified.csv  \
    2025_02_01_CR_S1_cast 1_modified.csv                                    0.117408   
    2025_02_03_CR_S2_cast 3_modified.csv                                    0.000328   
    2025_02_03_CR_S3d_cast 1_modified.csv                                   1.000000   
    2025_02_04_CR_S4_deep_cast 1_modified.csv                               1.000000   
    
                                               2025_02_04_CR_S4_deep_cast 1_modified.csv  
    2025_02_01_CR_S1_cast 1_modified.csv                                    1.705872e-08  
    2025_02_03_CR_S2_cast 3_modified.csv                                    4.563128e-11  
    2025_02_03_CR_S3d_cast 1_modified.csv                                   1.000000e+00  
    2025_02_04_CR_S4_deep_cast 1_modified.csv                               1.000000e+00  
    
    CTD_Samara_chla_ugL
                                               2025_02_01_CR_S1_cast 1_modified.csv  \
    2025_02_01_CR_S1_cast 1_modified.csv                                   1.000000   
    2025_02_03_CR_S2_cast 3_modified.csv                                   0.013741   
    2025_02_03_CR_S3d_cast 1_modified.csv                                  0.007156   
    2025_02_04_CR_S4_deep_cast 1_modified.csv                              0.000007   
    
                                               2025_02_03_CR_S2_cast 3_modified.csv  \
    2025_02_01_CR_S1_cast 1_modified.csv                               1.374131e-02   
    2025_02_03_CR_S2_cast 3_modified.csv                               1.000000e+00   
    2025_02_03_CR_S3d_cast 1_modified.csv                              1.000000e+00   
    2025_02_04_CR_S4_deep_cast 1_modified.csv                          3.648287e-09   
    
                                               2025_02_03_CR_S3d_cast 1_modified.csv  \
    2025_02_01_CR_S1_cast 1_modified.csv                                7.155532e-03   
    2025_02_03_CR_S2_cast 3_modified.csv                                1.000000e+00   
    2025_02_03_CR_S3d_cast 1_modified.csv                               1.000000e+00   
    2025_02_04_CR_S4_deep_cast 1_modified.csv                           1.466228e-08   
    
                                               2025_02_04_CR_S4_deep_cast 1_modified.csv  
    2025_02_01_CR_S1_cast 1_modified.csv                                    6.843814e-06  
    2025_02_03_CR_S2_cast 3_modified.csv                                    3.648287e-09  
    2025_02_03_CR_S3d_cast 1_modified.csv                                   1.466228e-08  
    2025_02_04_CR_S4_deep_cast 1_modified.csv                               1.000000e+00  
    
    ProDSS_Samara_pH
                                       020225_ProDSS_S1_modified.csv  \
    020225_ProDSS_S1_modified.csv                       1.000000e+00   
    020225_ProDSS_S2_modified.csv                       2.442809e-16   
    020325_ProDSS_S1_modified.csv                       2.925944e-08   
    020325_ProDSS_S3d_modified.csv                      1.185801e-03   
    020425_ProDSS_S1_modified.csv                       1.000000e+00   
    020425_ProDSS_S4deep_modified.csv                   1.525507e-47   
    
                                       020225_ProDSS_S2_modified.csv  \
    020225_ProDSS_S1_modified.csv                       2.442809e-16   
    020225_ProDSS_S2_modified.csv                       1.000000e+00   
    020325_ProDSS_S1_modified.csv                       1.000000e+00   
    020325_ProDSS_S3d_modified.csv                      3.008969e-03   
    020425_ProDSS_S1_modified.csv                       1.727959e-10   
    020425_ProDSS_S4deep_modified.csv                   3.527836e-05   
    
                                       020325_ProDSS_S1_modified.csv  \
    020225_ProDSS_S1_modified.csv                       2.925944e-08   
    020225_ProDSS_S2_modified.csv                       1.000000e+00   
    020325_ProDSS_S1_modified.csv                       1.000000e+00   
    020325_ProDSS_S3d_modified.csv                      1.000000e+00   
    020425_ProDSS_S1_modified.csv                       1.073378e-04   
    020425_ProDSS_S4deep_modified.csv                   5.136301e-09   
    
                                       020325_ProDSS_S3d_modified.csv  \
    020225_ProDSS_S1_modified.csv                        1.185801e-03   
    020225_ProDSS_S2_modified.csv                        3.008969e-03   
    020325_ProDSS_S1_modified.csv                        1.000000e+00   
    020325_ProDSS_S3d_modified.csv                       1.000000e+00   
    020425_ProDSS_S1_modified.csv                        1.791730e-01   
    020425_ProDSS_S4deep_modified.csv                    1.284577e-15   
    
                                       020425_ProDSS_S1_modified.csv  \
    020225_ProDSS_S1_modified.csv                       1.000000e+00   
    020225_ProDSS_S2_modified.csv                       1.727959e-10   
    020325_ProDSS_S1_modified.csv                       1.073378e-04   
    020325_ProDSS_S3d_modified.csv                      1.791730e-01   
    020425_ProDSS_S1_modified.csv                       1.000000e+00   
    020425_ProDSS_S4deep_modified.csv                   5.060315e-34   
    
                                       020425_ProDSS_S4deep_modified.csv  
    020225_ProDSS_S1_modified.csv                           1.525507e-47  
    020225_ProDSS_S2_modified.csv                           3.527836e-05  
    020325_ProDSS_S1_modified.csv                           5.136301e-09  
    020325_ProDSS_S3d_modified.csv                          1.284577e-15  
    020425_ProDSS_S1_modified.csv                           5.060315e-34  
    020425_ProDSS_S4deep_modified.csv                       1.000000e+00  


    /Users/kaylacayemitte/miniconda3/envs/CostaRica2025_env/lib/python3.11/site-packages/scipy/stats/_axis_nan_policy.py:586: UserWarning: scipy.stats.shapiro: Input data has range zero. The results may not be accurate.
      res = hypotest_fun_out(*samples, **kwds)

Calculating Averages and Standard Errors for each Data Frame 

 ---- CTD ----
ctd_metrics = ['TEMP', 'PSAL', 'density', 'chla_ugL']

# Calculate means
ctd_means = pd.DataFrame({
    'Samara Top': df_samara_top[ctd_metrics].mean(),
    'Samara Bottom': df_samara_bottom[ctd_metrics].mean(),
    'Papagayo Top': df_papagayo_top[ctd_metrics].mean(),
    'Papagayo Bottom': df_papagayo_bottom[ctd_metrics].mean()
}).T

Calculate standard errors
ctd_se = pd.DataFrame({
    'Samara Top': df_samara_top[ctd_metrics].std(ddof=1) / np.sqrt(len(df_samara_top)),
    'Samara Bottom': df_samara_bottom[ctd_metrics].std(ddof=1) / np.sqrt(len(df_samara_bottom)),
    'Papagayo Top': df_papagayo_top[ctd_metrics].std(ddof=1) / np.sqrt(len(df_papagayo_top)),
    'Papagayo Bottom': df_papagayo_bottom[ctd_metrics].std(ddof=1) / np.sqrt(len(df_papagayo_bottom))
}).T

Sample sizes
ctd_n = pd.DataFrame({
    'Samara Top': [len(df_samara_top)] * len(ctd_metrics),
    'Samara Bottom': [len(df_samara_bottom)] * len(ctd_metrics),
    'Papagayo Top': [len(df_papagayo_top)] * len(ctd_metrics),
    'Papagayo Bottom': [len(df_papagayo_bottom)] * len(ctd_metrics)
}, index=ctd_metrics).T

print("CTD Averages:")
print(ctd_means.round(3))
print("\nCTD Standard Errors:")
print(ctd_se.round(3))
print("\nCTD Sample Sizes:")
print(ctd_n)


---- ProDSS ----
prods_metrics = ['DO mg/L', 'pH']

Calculate means
prods_means = pd.DataFrame({
    'Samara Top': df_PRODSS_samara_top.groupby('source_file')[prods_metrics].mean().mean(),
    'Samara Bottom': df_PRODSS_samara_bottom.groupby('source_file')[prods_metrics].mean().mean(),
    'Papagayo Top': df_PRODSS_papagayo_top.groupby('source_file')[prods_metrics].mean().mean(),
    'Papagayo Bottom': df_PRODSS_papagayo_bottom.groupby('source_file')[prods_metrics].mean().mean()
}).T

 Calculate standard errors
prods_se = pd.DataFrame({
    'Samara Top': df_PRODSS_samara_top.groupby('source_file')[prods_metrics].mean().std(ddof=1) / np.sqrt(df_PRODSS_samara_top['source_file'].nunique()),
    'Samara Bottom': df_PRODSS_samara_bottom.groupby('source_file')[prods_metrics].mean().std(ddof=1) / np.sqrt(df_PRODSS_samara_bottom['source_file'].nunique()),
    'Papagayo Top': df_PRODSS_papagayo_top.groupby('source_file')[prods_metrics].mean().std(ddof=1) / np.sqrt(df_PRODSS_papagayo_top['source_file'].nunique()),
    'Papagayo Bottom': df_PRODSS_papagayo_bottom.groupby('source_file')[prods_metrics].mean().std(ddof=1) / np.sqrt(df_PRODSS_papagayo_bottom['source_file'].nunique())
}).T

 Sample sizes
prods_n = pd.DataFrame({
    'Samara Top': [len(df_PRODSS_samara_top)] * len(prods_metrics),
    'Samara Bottom': [len(df_PRODSS_samara_bottom)] * len(prods_metrics),
    'Papagayo Top': [len(df_PRODSS_papagayo_top)] * len(prods_metrics),
    'Papagayo Bottom': [len(df_PRODSS_papagayo_bottom)] * len(prods_metrics)
}, index=prods_metrics).T

print("\nProDSS Averages:")
print(prods_means.round(3))
print("\nProDSS Standard Errors:")
print(prods_se.round(3))
print("\nProDSS Sample Sizes:")
print(prods_n)


```python
# SD and SE

import numpy as np
import pandas as pd

def summarize_stats(df_dict, metrics):
    """
    df_dict: dict of {group_name: dataframe}
    metrics: list of metric columns
    """
    means = {}
    sds = {}
    ses = {}
    ns = {}

    for name, df in df_dict.items():
        means[name] = df[metrics].mean()
        sds[name] = df[metrics].std(ddof=1)
        ses[name] = sds[name] / np.sqrt(len(df))
        ns[name] = len(df)

    means = pd.DataFrame(means).T
    sds = pd.DataFrame(sds).T
    ses = pd.DataFrame(ses).T
    ns = pd.DataFrame(
        {k: [v] * len(metrics) for k, v in ns.items()},
        index=metrics
    ).T

    return means, sds, ses, ns
ctd_metrics = ['TEMP', 'PSAL', 'density', 'chla_ugL']

ctd_groups = {
    'Samara Top': df_samara_top,
    'Samara Bottom': df_samara_bottom,
    'Papagayo Top': df_papagayo_top,
    'Papagayo Bottom': df_papagayo_bottom
}

ctd_means, ctd_sd, ctd_se, ctd_n = summarize_stats(ctd_groups, ctd_metrics)

print("CTD Means")
print(ctd_means.round(3))

print("\nCTD Standard Deviations")
print(ctd_sd.round(3))

print("\nCTD Standard Errors")
print(ctd_se.round(3))

print("\nCTD Sample Sizes")
print(ctd_n)

prods_metrics = ['DO mg/L', 'pH']

prods_groups = {
    'Samara Top': df_PRODSS_samara_top,
    'Samara Bottom': df_PRODSS_samara_bottom,
    'Papagayo Top': df_PRODSS_papagayo_top,
    'Papagayo Bottom': df_PRODSS_papagayo_bottom
}

prods_means, prods_sd, prods_se, prods_n = summarize_stats(prods_groups, prods_metrics)

print("\nProDSS Means")
print(prods_means.round(3))

print("\nProDSS Standard Deviations")
print(prods_sd.round(3))

print("\nProDSS Standard Errors")
print(prods_se.round(3))

print("\nProDSS Sample Sizes")
print(prods_n)


```


```python
from zipfile import ZipFile
import pandas as pd
import numpy as np

# Load and summarize each CSV file separately
def summarize_each_file(zip_name):
    zipped = ZipFile(zip_name, 'r')
    summaries = []

    for name in zipped.namelist():
        if name.lower().endswith('.csv'):
            with zipped.open(name) as f:
                df = pd.read_csv(f)
                df_clean = df.select_dtypes(include=[np.number])  # Only numeric columns

                if not df_clean.empty:
                    mean_val = df_clean.mean().mean()  # Average of all column averages
                    std_val = df_clean.stack().std()   # Standard deviation across all values
                    summaries.append({
                        'File': name,
                        'Average of Means': round(mean_val, 3),
                        'Standard Deviation': round(std_val, 3),
                        'Sample Size': df_clean.size
                    })

    return pd.DataFrame(summaries)

# Run for both CTD and ProDSS
ctd_summary = summarize_each_file('CTD.zip')
prods_summary = summarize_each_file('ProDSS.zip')

# Label the source
ctd_summary['Source'] = 'CTD'
prods_summary['Source'] = 'ProDSS'

# Combine results
summary_table = pd.concat([ctd_summary, prods_summary], ignore_index=True)

# Show result
print(summary_table)

```


```python
#Getting the averages of sites 
files = [
    "020225_ProDSS_S1",
    "020225_ProDSS_S2",
    "020325_ProDSS_S3d",
    "020325_ProDSS_S1",
    "020425_ProDSS_S4deep",
    "020425_ProDSS_S1",
    "020625_ProDSS_P1"
]

results = []

for f in files:
    df_f = df_PRODSS_samara_bottom[
        df_PRODSS_samara_bottom['source_file'].str.contains(f, na=False)
    ]

    results.append({
        "file": f,
        "n": len(df_f),
        "DO mg/L mean": df_f["DO mg/L"].mean(),
        "pH mean": df_f["pH"].mean()
    })

df_summary = pd.DataFrame(results)

df_summary
```




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
      <th>file</th>
      <th>n</th>
      <th>DO mg/L mean</th>
      <th>pH mean</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>020225_ProDSS_S1</td>
      <td>51</td>
      <td>7.859412</td>
      <td>8.091176</td>
    </tr>
    <tr>
      <th>1</th>
      <td>020225_ProDSS_S2</td>
      <td>49</td>
      <td>7.118571</td>
      <td>8.025306</td>
    </tr>
    <tr>
      <th>2</th>
      <td>020325_ProDSS_S3d</td>
      <td>34</td>
      <td>7.171471</td>
      <td>8.050000</td>
    </tr>
    <tr>
      <th>3</th>
      <td>020325_ProDSS_S1</td>
      <td>36</td>
      <td>8.238889</td>
      <td>8.040000</td>
    </tr>
    <tr>
      <th>4</th>
      <td>020425_ProDSS_S4deep</td>
      <td>99</td>
      <td>6.230606</td>
      <td>7.986364</td>
    </tr>
    <tr>
      <th>5</th>
      <td>020425_ProDSS_S1</td>
      <td>45</td>
      <td>8.001556</td>
      <td>8.080000</td>
    </tr>
    <tr>
      <th>6</th>
      <td>020625_ProDSS_P1</td>
      <td>0</td>
      <td>NaN</td>
      <td>NaN</td>
    </tr>
  </tbody>
</table>
</div>




```python
import pandas as pd
import numpy as np

# --------------------------
# VARIABLES
# --------------------------
ctd_vars = ["TEMP", "PSAL", "chla_ugL"]
prodss_vars = ["pH"]

# --------------------------
# FUNCTION TO COMPUTE METRICS
# --------------------------
def summary_stats(df, vars_list):
    return pd.DataFrame({
        "Mean": df[vars_list].mean(),
        "StdDev": df[vars_list].std(ddof=1),
        "SE": df[vars_list].std(ddof=1) / np.sqrt(len(df)),
        "N": df[vars_list].count()
    })

# --------------------------
# CTD: SAMARA + PAPAGAYO
# --------------------------
ctd_samara_stats = summary_stats(df_samara_bottom, ctd_vars)
ctd_papagayo_stats = summary_stats(df_papagayo_bottom, ctd_vars)

ctd_samara_stats.index.name = "Variable"
ctd_papagayo_stats.index.name = "Variable"

print("\nCTD - Samara (Bottom 1 m)")
print(ctd_samara_stats.round(3))

print("\nCTD - Papagayo (Bottom 1 m)")
print(ctd_papagayo_stats.round(3))

# --------------------------
# ProDSS: SAMARA + PAPAGAYO
# --------------------------
prods_samara_stats = summary_stats(df_PRODSS_samara_bottom, prodss_vars)
prods_papagayo_stats = summary_stats(df_PRODSS_papagayo_bottom, prodss_vars)

prods_samara_stats.index.name = "Variable"
prods_papagayo_stats.index.name = "Variable"

print("\nProDSS - Samara (Bottom 1 m)")
print(prods_samara_stats.round(3))

print("\nProDSS - Papagayo (Bottom 1 m)")
print(prods_papagayo_stats.round(3))
```

    
    CTD - Samara (Bottom 1 m)
                Mean  StdDev     SE   N
    Variable                           
    TEMP      28.648   0.962  0.113  72
    PSAL      32.724   0.301  0.035  72
    chla_ugL   3.578   1.066  0.126  72
    
    CTD - Papagayo (Bottom 1 m)
                Mean  StdDev     SE    N
    Variable                            
    TEMP      25.452   0.643  0.064  100
    PSAL      33.869   0.121  0.012  100
    chla_ugL  12.477   5.377  0.538  100
    
    ProDSS - Samara (Bottom 1 m)
               Mean  StdDev     SE    N
    Variable                           
    pH        8.036   0.041  0.002  314
    
    ProDSS - Papagayo (Bottom 1 m)
              Mean  StdDev     SE    N
    Variable                          
    pH        7.96    0.05  0.003  328



```python

```
