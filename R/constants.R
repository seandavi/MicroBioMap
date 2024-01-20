.versions <- c(
    '1.0.0',
    '1.0.1'
)
# Describes the IDs assigned by Zenodo to each compendium release.
# Should be determined from the release's URL:
# https://zenodo.org/records/{ID IS HERE}
.record_ids <- c(
    '8431937',  # 1.0.0
    '10452633'  # 1.0.1
)

# Uses the data above to build URLs for the files needed
.data_url <- paste0('https://zenodo.org/record/', .record_ids, '/files/taxonomic_table.csv.gz')
names(.data_url) <- .versions

.coldata_url <- paste0('https://zenodo.org/record/', .record_ids, '/files/sample_metadata.tsv')
names(.coldata_url) <- .versions
