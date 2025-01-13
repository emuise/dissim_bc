import os
import rasterio
import numpy as np
from sklearn.decomposition import IncrementalPCA

# Define paths
tiles_folder = "E:/Sync/Masters/05_dissim_bc/data/rasters/tiles"
output_folder = "E:/Sync/Masters/05_dissim_bc/data/rasters/tiles_pca"
os.makedirs(output_folder, exist_ok=True)

# Define IPCA models
ipca_all = IncrementalPCA(n_components=6)
ipca_function = IncrementalPCA(n_components=2)
ipca_structure = IncrementalPCA(n_components=4)

# Initialize accumulators for the IPCA
ipca_models = {'all': ipca_all, 'function': ipca_function, 'structure': ipca_structure}
layers_indices = {'all': slice(-6, None), 'function': slice(-2, None), 'structure': slice(-6, -2)}

# Define a minimum data threshold for PCA
min_data_points = 100  # Adjust based on your data's characteristics

# Initialize a buffer for merging chunks
data_buffer = {group: [] for group in ipca_models.keys()}

# Function to check if there is enough data in the buffer
def buffer_has_enough_data(buffer, min_points):
    return buffer.shape[0] >= min_points

files_iter = os.listdir(tiles_folder)

# sort them so that i have an idea of how far into processing it is :)
files_iter.sort(key = lambda f: int("".join(filter(str.isdigit, f))))

for tile_file in files_iter:
    if tile_file.endswith(".dat"):
        tile_path = os.path.join(tiles_folder, tile_file)
        print(tile_path)
        
        # Load the raster tile
        with rasterio.open(tile_path) as src:
            data = src.read()  # Shape: (bands, height, width)

        # Ensure the data is reshaped into (samples, features)
        reshaped_data = data.reshape(data.shape[0], -1).T  # Shape: (pixels, bands)
        

        # Run IPCA for each group
        for group, model in ipca_models.items():
            # subset = reshaped_data[:, slice(-6,None)]
            subset = reshaped_data[:, layers_indices[group]]  # Extract relevant layers

            # Remove rows with NaN values
            valid_data_mask = ~np.isnan(subset).any(axis=1)
            subset = subset[valid_data_mask]

            # Add the subset to the buffer
            data_buffer[group].append(subset)

            # Combine all buffered chunks for this group
            combined_data = np.vstack(data_buffer[group])

            # Check if there is enough data for PCA
            if buffer_has_enough_data(combined_data, min_data_points):
                model.partial_fit(combined_data)

                # Clear the buffer after processing
                data_buffer[group] = []
                print()

# Handle any remaining data in buffers
for group, model in ipca_models.items():
    if data_buffer[group]:
        combined_data = np.vstack(data_buffer[group])
        if buffer_has_enough_data(combined_data, min_data_points):
            model.partial_fit(combined_data)
        else:
            print("GET ABSOLUTELY REKT, have fun figuring this out") 

# Save transformed data for each chunk (updated with PCA components and logic for merging)
for tile_file in files_iter:
    if tile_file.endswith('.tif'):
        tile_path = os.path.join(tiles_folder, tile_file)
        print(tile_path)

        # Load the raster tile
        with rasterio.open(tile_path) as src:
            data = src.read()  # Shape: (bands, height, width)
            profile = src.profile

        # Reshape the data to (samples, features)
        reshaped_data = data.reshape(data.shape[0], -1).T  # Shape: (pixels, bands)

        # Replace NaN values with 0 for transformation
        reshaped_data = np.nan_to_num(reshaped_data, nan=0)

        # Transform data for each group
        for group, model in ipca_models.items():
            subset = reshaped_data[:, layers_indices[group]]  # Extract relevant layers

            # Transform the data
            transformed = model.transform(subset)

            # Reshape the transformed data back to spatial dimensions
            transformed_data = transformed.T.reshape(-1, data.shape[1], data.shape[2])

            # Update profile for output
            profile.update(count=transformed_data.shape[0], dtype=transformed_data.dtype)

            # Save the transformed data
            output_path = os.path.join(output_folder, f"{os.path.splitext(tile_file)[0]}_{group}.dat")
            with rasterio.open(output_path, 'w', **profile) as dst:
                dst.write(transformed_data)