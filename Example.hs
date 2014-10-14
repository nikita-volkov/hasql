


main =
  do
    H.withPool $ \pool -> do
      artistID <- 
        join $ fmap ListT.head $ H.runExecutor pool $ H.selectExecutor $ 
          [H.q| SELECT id FROM artists WHERE name = ? |] 
            "Metallica"
      userID <-
        join $ fmap ListT.head $ H.runExecutor pool $ H.selectExecutor $ 
          [H.q| SELECT id FROM users WHERE name = ? |] 
            "Nikita Volkov"

      undefined



main =
  do
    H.withPool $ \pool -> do
      H.runExecutor pool $ do
        H.writeTransactionExecutor H.Serialized $ do
          artistIDMaybe <- 
            H.selectTransaction $
              [H.q| SELECT id FROM artists WHERE name = ? |] 
                "Metallica"
          userIDMaybe <-
            H.selectTransaction $
              [H.q| SELECT id FROM users WHERE name = ? |] 
                "Nikita Volkov"
          forM_ ((,) <$> artistIDMaybe <*> userIDMaybe) $ \(artistID, userID) -> do
            H.insertTransaction $
              [H.q| INSERT INTO artists_fans (artist_id, user_id) VALUES (?, ?) |]
                artistID
                userID

main =
  do
    H.withPool $ \pool -> do
      H.txIO pool (H.NoACID) $ do
        H.noResultTx $
          [H.q| INSERT INTO users (name, birth_date, gender) VALUES (?, ?, ?) |]
            "Nikita Volkov"
            "1358-10-12"
            (H.Enum Male)

      H.txIO pool (H.Write H.Serialized) $ do
        artistID <-
          H.streamWithCursorTx $
            [H.q| SELECT id FROM artists WHERE name = ? |] 
              ("Metallica")


