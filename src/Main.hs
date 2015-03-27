import MusicReader
main = do
        file <- getLine
        metadata <- readMusicMetadata file
        putStrLn . show $ metadata
