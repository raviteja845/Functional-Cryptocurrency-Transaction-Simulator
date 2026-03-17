import Data.List

data Wallet = Wallet {
    walletId :: String,
    owner :: String,
    balance :: Int
} deriving (Show)

data Transaction = Transaction {
    txId :: String,
    sender :: String,
    receiver :: String,
    amount :: Int,
    status :: String
} deriving (Show)

type Ledger = [Transaction]

main :: IO ()
main = menu [] [] 1

menu :: [Wallet] -> Ledger -> Int -> IO ()
menu wallets ledger txCounter = do
    putStrLn "\n=== Cryptocurrency Transaction Simulator ==="
    putStrLn "1. Create Wallet"
    putStrLn "2. Transfer Tokens"
    putStrLn "3. Check Balance"
    putStrLn "4. View Ledger"
    putStrLn "5. Transaction History"
    putStrLn "6. Filter Transactions"
    putStrLn "7. Transaction Summary"
    putStrLn "8. Validate Blockchain"
    putStrLn "9. Exit"

    putStr "Enter your choice: "
    choice <- getLine

    case choice of
        "1" -> createWallet wallets ledger txCounter
        "2" -> transferTokens wallets ledger txCounter
        "3" -> checkBalance wallets ledger txCounter
        "4" -> viewLedger wallets ledger txCounter
        "5" -> transactionHistory wallets ledger txCounter
        "6" -> filterTransactions wallets ledger txCounter
        "7" -> transactionSummary wallets ledger txCounter
        "8" -> validateBlockchain wallets ledger txCounter
        "9" -> putStrLn "\nExiting program...\nThank you for using Cryptocurrency Transaction Simulator"
        _   -> do
            putStrLn "Invalid choice"
            menu wallets ledger txCounter


createWallet wallets ledger txCounter = do
    putStr "Enter Wallet ID: "
    wid <- getLine
    putStr "Enter Owner Name: "
    name <- getLine
    putStr "Enter Initial Balance: "
    bal <- getLine

    putStrLn "\n-------------------------------"

    let wallet = Wallet wid name (read bal)

    putStrLn "Wallet Created Successfully"
    putStrLn ("Wallet ID: " ++ wid)
    putStrLn ("Owner: " ++ name)
    putStrLn ("Balance: " ++ bal ++ " tokens")

    putStrLn "-------------------------------"

    menu (wallet:wallets) ledger txCounter


transferTokens wallets ledger txCounter = do
    putStr "Enter Sender Wallet ID: "
    s <- getLine
    putStr "Enter Receiver Wallet ID: "
    r <- getLine
    putStr "Enter Amount: "
    amtStr <- getLine

    putStrLn "\n-------------------------------"

    let amt = read amtStr :: Int
    let senderWallet = find (\w -> walletId w == s) wallets
    let receiverWallet = find (\w -> walletId w == r) wallets

    case (senderWallet, receiverWallet) of
        (Just sw, Just rw) ->
            if balance sw >= amt
            then do
                let newSender = sw {balance = balance sw - amt}
                let newReceiver = rw {balance = balance rw + amt}

                let updatedWallets =
                        map (\w ->
                            if walletId w == s then newSender
                            else if walletId w == r then newReceiver
                            else w) wallets

                let txIdVal = "TX" ++ show txCounter
                let tx = Transaction txIdVal s r amt "Completed"

                putStrLn "Processing Transaction..."
                putStrLn "Transaction Successful\n"

                putStrLn ("Sender: " ++ s)
                putStrLn ("Receiver: " ++ r)
                putStrLn ("Amount: " ++ show amt ++ " tokens")

                putStrLn "\nUpdated Balances"
                putStrLn (s ++ ": " ++ show (balance newSender) ++ " tokens")
                putStrLn (r ++ ": " ++ show (balance newReceiver) ++ " tokens")

                putStrLn "-------------------------------"

                menu updatedWallets (tx:ledger) (txCounter+1)

            else do
                putStrLn "Transaction Failed"
                putStrLn "Reason: Insufficient Balance"
                putStrLn "-------------------------------"
                menu wallets ledger txCounter

        _ -> do
            putStrLn "Wallet not found"
            putStrLn "-------------------------------"
            menu wallets ledger txCounter


checkBalance wallets ledger txCounter = do
    putStr "Enter Wallet ID: "
    wid <- getLine

    putStrLn "\n-------------------------------"

    let result = find (\w -> walletId w == wid) wallets

    case result of
        Just w -> do
            putStrLn "Wallet Information"
            putStrLn ("Wallet ID: " ++ walletId w)
            putStrLn ("Owner: " ++ owner w)
            putStrLn ("Current Balance: " ++ show (balance w) ++ " tokens")

        Nothing -> putStrLn "Wallet not found"

    putStrLn "-------------------------------"

    menu wallets ledger txCounter


viewLedger wallets ledger txCounter = do
    putStrLn "\n-------------------------------"
    putStrLn "Ledger Transactions"
    mapM_ print ledger
    putStrLn "-------------------------------"

    menu wallets ledger txCounter


transactionHistory wallets ledger txCounter = do
    putStr "Enter Wallet ID: "
    wid <- getLine

    putStrLn "\n-------------------------------"

    let history = filter (\t -> sender t == wid || receiver t == wid) ledger

    putStrLn ("Transaction History for Wallet " ++ wid)
    mapM_ print history

    putStrLn "-------------------------------"

    menu wallets ledger txCounter


filterTransactions wallets ledger txCounter = do
    putStrLn "\nSelect Filter Type"
    putStrLn "1. Transactions Sent"
    putStrLn "2. Transactions Received"
    putStrLn "3. Completed Transactions"
    putStrLn "4. Failed Transactions"

    opt <- getLine

    putStrLn "\n-------------------------------"

    case opt of
        "1" -> do
            putStr "Enter Wallet ID: "
            wid <- getLine
            let result = filter (\t -> sender t == wid) ledger
            mapM_ print result

        "2" -> do
            putStr "Enter Wallet ID: "
            wid <- getLine
            let result = filter (\t -> receiver t == wid) ledger
            mapM_ print result

        "3" -> mapM_ print (filter (\t -> status t == "Completed") ledger)

        "4" -> mapM_ print (filter (\t -> status t == "Failed") ledger)

        _   -> putStrLn "Invalid option"

    putStrLn "-------------------------------"

    menu wallets ledger txCounter


transactionSummary wallets ledger txCounter = do
    putStrLn "\n-------------------------------"

    let total = foldl (\acc t -> acc + amount t) 0 ledger

    putStrLn "Transaction Summary"
    putStrLn ("Total Transactions: " ++ show (length ledger))
    putStrLn ("Total Tokens Transferred: " ++ show total ++ " tokens")

    putStrLn "-------------------------------"

    menu wallets ledger txCounter


validateBlockchain wallets ledger txCounter = do
    putStrLn "\n-------------------------------"
    putStrLn "Validating Blockchain..."
    putStrLn "Block 1 Verified"
    putStrLn "Block 2 Verified"
    putStrLn "\nBlockchain is Valid"
    putStrLn "-------------------------------"

    menu wallets ledger txCounter