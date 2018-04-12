# Data Cleansing Decisions
Instead of removing holes and outliers, we decided to keep these values in order to retain as much accuracy as possible when our algorithm is being presented with new data.
To fill the holes, we calcualted what the mean would be for each column if the holes were exlcuded from the data, then used that value to fill all of the holes for their respective column.
This way we are able to have the same amount of data points as the original dataset.