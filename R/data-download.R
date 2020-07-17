library(osfr)
dir.create("data")
osf_retrieve_node("xykzv") %>%
        osf_ls_files(n_max = Inf) %>%
        osf_download(path = "data",
                     conflicts = "overwrite")
