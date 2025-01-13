import xarray as xr
import os
import numpy as np
import pandas as pd

# List of raster file names
covs = ["DEM.dat", "slope.dat", "clim_MWMT.dat", "clim_MCMT.dat", "clim_MAT.dat", "clim_MAP.dat"]

# Directory containing the raster files
clim_dir = "E:/Sync/Masters/05_dissim_bc/data/inputs"

# Full paths to each raster file
cov_paths = [os.path.join(clim_dir, cov) for cov in covs]

# Define quantile range
quantile_range = np.arange(0, 1.1, 0.2)

# Initialize an empty list to collect results
results = []

# Process each raster file
for file in cov_paths:
    print(f"Processing file: {file}")
    
    # Extract variable name from file name (excluding extension)
    var = os.path.splitext(os.path.basename(file))[0]
    
    # Open raster as an xarray DataArray and convert to an array
    rast = xr.open_dataset(file, engine="rasterio").to_array()
    
    # Compute quantiles
    q = rast.quantile(quantile_range)
    
    # Append quantile results to the list
    for quantile, value in zip(quantile_range, q.values):
        results.append({"variable": var, "quantile": quantile, "value": value})

# Convert the results to a Pandas DataFrame
df = pd.DataFrame(results)

# Save the DataFrame to a CSV file
output_csv_path = "E:/Sync/Masters/05_dissim_bc/data/quantiles.csv"
df.to_csv(output_csv_path, index=False)

print(f"Quantiles saved to {output_csv_path}")
