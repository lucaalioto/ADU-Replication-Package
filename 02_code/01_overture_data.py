import os
import subprocess
import geopandas as gpd
import matplotlib.pyplot as plt
import json

# Step 1: Set Sacramento bounding box: (west, south, east, north)
bbox = "-121.56,38.44,-121.31,38.7"  # Approximate bounding box of Sacramento

# Step 2: Download POI data for Sacramento as GeoJSON
output_geojson = "sacramento_pois.geojson"
command = f"overturemaps download --bbox={bbox} -f geojson --type=place -o {output_geojson}"

# Run the command and download the data
subprocess.run(command, shell=True, check=True)

# Step 3: Load GeoJSON using GeoPandas
gdf = gpd.read_file(output_geojson)

# Step 4: Extract name and categories using JSON parsing
def extract_name(names):
    try:
        names = names.replace("\\/", "/")  # Fix escape sequences
        names_dict = json.loads(names) if isinstance(names, str) else names
        return names_dict.get("primary", "Unknown")
    except (json.JSONDecodeError, TypeError, AttributeError):
        return "Unknown"

def extract_primary_category(categories):
    try:
        categories = categories.replace("\\/", "/")  # Fix escape sequences
        categories_dict = json.loads(categories) if isinstance(categories, str) else categories
        return categories_dict.get("primary", "Unknown")
    except (json.JSONDecodeError, TypeError, AttributeError):
        return "Unknown"
    
def extract_add(addresses): 
    try: 
        addresses = addresses.replace("\\/", "/")  # Fix escape sequences
        addresses_dict = json.loads(addresses) if isinstance(addresses, str) else addresses
        return addresses_dict.get("primary", "Unknown")
    except (json.JSONDecodeError, TypeError, AttributeError):
        return "Unknown"
    
def extract_add(addresses): 
    try: 
        addresses = addresses.replace("\\/", "/")  # Fix escape sequences
        addresses_list = json.loads(addresses) if isinstance(addresses, str) else addresses
        if isinstance(addresses_list, list) and len(addresses_list) > 0:
            return addresses_list[0].get("freeform", "Unknown")  # Extract 'freeform' field
        return "Unknown"
    except (json.JSONDecodeError, TypeError, AttributeError, IndexError):
        return "Unknown"


# Add 'name' and 'category' columns
gdf['name'] = gdf['names'].apply(extract_name)
gdf['category'] = gdf['categories'].apply(extract_primary_category)
gdf['address'] = gdf['addresses'].apply(extract_add)

# Step 5: Extract latitude and longitude from geometry
gdf['X'] = gdf.geometry.x
gdf['Y'] = gdf.geometry.y

# Step 6: Save the full dataset with all columns
full_output_csv = "sacramento_full_dataset.csv"
gdf.to_csv(full_output_csv, index=False)

# Step 7: Filter grocery stores based on categories
grocery_stores = gdf[gdf['category'].str.contains('grocery_store|supermarket', case=False, na=False)].copy()

# Step 8: Save the filtered grocery stores to a separate CSV
grocery_stores_output_csv = "sacramento_grocery_stores.csv"
grocery_stores[['id', 'name', 'category', 'address', 'X', 'Y']].to_csv(grocery_stores_output_csv, index=False)

print(f"Full dataset saved to {full_output_csv}")
print(f"Grocery stores dataset saved to {grocery_stores_output_csv}")

# Step 9: Plot the grocery stores on a map
base_map = gdf.plot(figsize=(10, 8), color='lightgray', alpha=0.5, edgecolor='white', linewidth=0.5)
grocery_stores.plot(ax=base_map, color='red', markersize=10, label='Grocery Stores', alpha=0.7)

# Customizing the map
plt.title("Grocery Stores in Sacramento", fontsize=14)
plt.xlabel("Longitude", fontsize=10)
plt.ylabel("Latitude", fontsize=10)
plt.legend()
plt.show()
