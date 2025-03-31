
import os
import shutil
# Create output directory if it doesn't exist
output_dir = "renamed_grms"
os.makedirs(output_dir, exist_ok=True)
# Find all .grm.id files in the current directory
grm_id_files = [f for f in os.listdir() if f.endswith("_grm.grm.id")]
for grm_id_file in grm_id_files:
    base_name = grm_id_file.replace("_grm.grm.id", "")  # Get prefix (e.g., "allPolys_ImputedNoRef")
    # Define paths for associated files
    grm_bin_file = f"{base_name}_grm.grm.bin"
    grm_n_bin_file = f"{base_name}_grm.grm.N.bin"
    grm_log_file = f"{base_name}_grm.log"
    # Rename and update .grm.id file
    new_grm_id_path = os.path.join(output_dir, grm_id_file)
    with open(grm_id_file, "r") as infile, open(new_grm_id_path, "w") as outfile:
        for line in infile:
            parts = line.strip().split()
            if len(parts) != 2:
                print(f"Skipping malformed line in {grm_id_file}: {line.strip()}")
                continue
            original_id = parts[1]
            if "_" in original_id:
                fid, iid = original_id.split("_", 1)  # Split on first underscore
            else:
                fid, iid = "0", original_id  # If no underscore, assume no FID
            outfile.write(f"{fid} {iid}\n")
    # Move other associated GRM files
    for file in [grm_bin_file, grm_n_bin_file, grm_log_file]:
        if os.path.exists(file):
            shutil.copy(file, output_dir)
            print(f"Copied {file} -> {output_dir}/")
    print(f"Updated {grm_id_file} -> {new_grm_id_path}")
print("All GRM files have been updated and moved to 'renamed_grms'.")
