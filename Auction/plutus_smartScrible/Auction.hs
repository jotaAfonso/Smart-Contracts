import           Control.Lens 
import           Control.Monad                         (void, forever) 
import qualified Data.ByteString.Char8                 as C 
import           Data.Aeson                            (FromJSON, ToJSON) 
import           Control.Monad.Error.Lens              (catching, throwing, throwing_) 
import           GHC.Generics                          (Generic) 
import qualified PlutusTx                              as PlutusTx 
import           PlutusTx.Prelude
import           Ledger                                hiding (to,initialise)
import qualified Ledger.Ada                            as Ada  
import qualified Ledger.Value                          as V
import           Ledger.Contexts                       as Cont
import           Ledger.Constraints                    (TxConstraints)  
import qualified Ledger.Constraints                    as Constraints 
import qualified Ledger.Typed.Scripts                  as Scripts  
import           Ledger.Typed.Tx                       (tyTxOutData,TypedScriptTxOut(..)) 
import           Plutus.Contract.StateMachine          (AsSMContractError (..), OnChainState, State (..), Void) 
import qualified Plutus.Contract.StateMachine          as SM 
import           Data.Text                             (Text) 
import qualified Data.Text                             as T 
import qualified Data.Map                              as Map
import           Data.String (fromString)
import           Plutus.Contract 
import           Playground.Contract
import           Ledger.AddressMap                    (UtxoMap) 
import           Plutus.Contract.Util                 (loopM) 
import qualified Prelude
import           Prelude                              (String, undefined, show)

-- | This contract was generated automatically using a scribble protocol as source:


-- | Declaration of the possible states for the State Machine:
data AuctionState =
    None [(Integer,(POSIXTime,Slot))]
    | InitialiseState Integer Integer Integer Integer Integer [(Integer,(POSIXTime,Slot))]
    | RunAuctionState Integer Integer Integer Integer Integer [(Integer,(POSIXTime,Slot))]
    | BidState Integer Integer Integer Integer Integer [(Integer,(POSIXTime,Slot))]
    | StopBiddingState Integer Integer Integer Integer Integer [(Integer,(POSIXTime,Slot))]
    | SellState Integer Integer Integer Integer Integer [(Integer,(POSIXTime,Slot))]
    | NotSellState Integer Integer Integer Integer Integer [(Integer,(POSIXTime,Slot))]
    deriving stock (Show, Generic)

-- | Declaration of the inputs that will be used for the transitions of the State Machine
data AuctionInput =
    InitialiseInput Integer Integer Integer Integer Integer [(Integer,(POSIXTime,Slot))] Value
    | RunAuctionInput Integer Integer Integer Integer Integer [(Integer,(POSIXTime,Slot))] Value
    | BidInput Integer Integer Integer Integer Integer [(Integer,(POSIXTime,Slot))] Value
    | BiddingInput Integer Integer Integer Integer Integer [(Integer,(POSIXTime,Slot))] Value
    | StopBiddingInput Integer Integer Integer Integer Integer [(Integer,(POSIXTime,Slot))] Value
    | SellInput Integer Integer Integer Integer Integer [(Integer,(POSIXTime,Slot))] Value
    | NotSellInput Integer Integer Integer Integer Integer [(Integer,(POSIXTime,Slot))] Value
    deriving stock (Show, Generic)

-- | Make the types possible to use in the "on chain" part of Plutus (State Machine)

PlutusTx.makeLift ''AuctionState
PlutusTx.makeLift ''AuctionInput
PlutusTx.unstableMakeIsData ''AuctionState
PlutusTx.unstableMakeIsData ''AuctionInput
-- | Declaration of the errors that will be used throughout this contract
data AuctionError = Error Text
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''AuctionError


-- | Transforming errors into Text to comply with endpoint definitions
mapSMError' :: Contract w s SM.SMContractError a -> Contract w s Text a
mapSMError' = mapError $ T.pack . show

mapContractError' :: Contract w s ContractError a -> Contract w s Text a
mapContractError' = mapError $ T.pack . show

-- | Contract schema: Endpoint and the parameters they receive
type AuctionSchema =
        Endpoint "bid" ()
        .\/ Endpoint "bidding" ()
        .\/ Endpoint "initialise" InitialiseParams
        .\/ Endpoint "notsell" ()
        .\/ Endpoint "runauction" RunAuctionParams
        .\/ Endpoint "sell" ()
        .\/ Endpoint "stopbidding" ()


data InitialiseParams = InitialiseParams {
  mPrice :: Integer,
  tPrice :: Integer,
  bA :: Integer,
  bB :: Integer,
  cBid :: Integer
  } 
  deriving stock (Prelude.Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

data RunAuctionParams = RunAuctionParams {
  offerPriceValue :: Integer
  } 
  deriving stock (Prelude.Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

stringToKey :: String -> PaymentPubKeyHash
stringToKey key = PaymentPubKeyHash{unPaymentPubKeyHash = fromString key}

{-# INLINABLE validateKeys #-}
validateKeys :: [PaymentPubKeyHash] -> TxConstraints Void Void
validateKeys keys = if null keys
    then mempty
    else Constraints.mustSatisfyAnyOf $ map (Constraints.mustBeSignedBy) keys

-- | State Machine transactions and client definition
{-# INLINABLE transition #-}
transition :: State AuctionState -> AuctionInput -> Maybe (TxConstraints Void Void, State AuctionState)
transition State{stateData=oldData,stateValue} input = case (oldData, input) of
    (None _, InitialiseInput minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps stateVal) -> Just(mempty, State{stateData = InitialiseState minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps, stateValue = stateVal})
    (BidState _ _ _ _ _ _, BiddingInput minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps stateVal) -> Just(mempty, State{stateData = RunAuctionState minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps, stateValue = stateVal})
    (InitialiseState _ _ _ _ _ _, RunAuctionInput minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps stateVal) -> Just(mempty, State{stateData = RunAuctionState minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps, stateValue = stateVal})
    (RunAuctionState _ _ _ _ _ _, StopBiddingInput minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps stateVal) -> Just(mempty, State{stateData = StopBiddingState minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps, stateValue = stateVal})
    (RunAuctionState _ _ _ _ _ _, BidInput minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps stateVal) -> Just(mempty, State{stateData = BidState minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps, stateValue = stateVal})
    (StopBiddingState _ _ _ _ _ _, SellInput minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps stateVal) -> Just(mempty, State{stateData = SellState minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps, stateValue = stateVal})
    (SellState _ _ _ _ _ _, NotSellInput minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps stateVal) -> Just(mempty, State{stateData = NotSellState minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps, stateValue = mempty})
    
    _ -> Nothing

{-# INLINABLE transitionCheck #-}
transitionCheck :: AuctionState -> AuctionInput -> ScriptContext -> Bool
transitionCheck state input context = case (state, input) of
    (None triggerTimeStamps, InitialiseInput _ _ _ _ _ _ _) -> True
    (BidState minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps, BiddingInput _ _ _ _ _ _ _) -> True
    (InitialiseState minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps, RunAuctionInput _ _ _ _ _ _ _) -> True
    (RunAuctionState minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps, StopBiddingInput _ _ _ _ _ _ _) -> True
    (RunAuctionState minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps, BidInput _ _ _ _ _ _ _) -> True
    (StopBiddingState minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps, SellInput _ _ _ _ _ _ _) -> True
    (SellState minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps, NotSellInput _ _ _ _ _ _ _) -> True
    
    _ -> False
    where
        checkKeys keys = any (txSignedBy $ (scriptContextTxInfo context)) $ map unPaymentPubKeyHash keys

{-# INLINABLE machine #-}
machine :: SM.StateMachine AuctionState AuctionInput
machine = SM.StateMachine
        { SM.smTransition = transition
        , SM.smFinal = isFinal
        , SM.smCheck = transitionCheck
        , SM.smThreadToken = Nothing
        }
    where
        isFinal (NotSellState _ _ _ _ _ _) = True
        isFinal _       = False

{-# INLINABLE mkValidator #-} 
mkValidator :: Scripts.ValidatorType (SM.StateMachine AuctionState AuctionInput)
mkValidator = SM.mkValidator machine

scriptInstance :: Scripts.TypedValidator (SM.StateMachine AuctionState AuctionInput)
scriptInstance = Scripts.mkTypedValidator @(SM.StateMachine AuctionState AuctionInput)
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @AuctionState @AuctionInput

machineInstance :: SM.StateMachineInstance AuctionState AuctionInput
machineInstance = SM.StateMachineInstance machine scriptInstance

client :: SM.StateMachineClient AuctionState AuctionInput
client = SM.mkStateMachineClient machineInstance

-- | Methods to interact with the state machine

runContractStep :: SM.StateMachineClient AuctionState AuctionInput -> AuctionInput -> TxConstraints AuctionInput AuctionState -> Contract () AuctionSchema SM.SMContractError ( Maybe AuctionState )
runContractStep client input constraint = 
    do 
    pkh <- ownPaymentPubKeyHash
    r <- SM.runStepWith (Constraints.ownPaymentPubKeyHash pkh) constraint client input 
    case r of 
      SM.TransitionFailure i -> pure Nothing
      SM.TransitionSuccess s -> pure (Just s)

initialiseSM :: Contract () AuctionSchema SM.SMContractError (Maybe AuctionState)
initialiseSM = do
    currentState <- getCurrentStateSM client
    case currentState of
        Nothing -> do
          let triggerTimeStamps = []
          SM.runInitialise client (None  triggerTimeStamps) mempty
          pure Nothing
        x -> pure x

getCurrentStateSM :: SM.StateMachineClient AuctionState AuctionInput -> Contract () AuctionSchema SM.SMContractError (Maybe AuctionState)
getCurrentStateSM client = do
  currentState <- SM.getOnChainState client
  case currentState of
    Just (SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=state}}, _) -> pure (Just (state))
    _ -> pure Nothing

isValidCallInState :: Maybe AuctionState -> AuctionInput -> Bool
isValidCallInState Nothing input = canInitialiseSM input
isValidCallInState (Just state) input = validTransitions state input

canInitialiseSM :: AuctionInput -> Bool
canInitialiseSM (InitialiseInput _ _ _ _ _ _ _) = True
canInitialiseSM _ = False

validTransitions :: AuctionState -> AuctionInput -> Bool
validTransitions (SellState _ _ _ _ _ _) (NotSellInput _ _ _ _ _ _ _) = True
validTransitions (StopBiddingState _ _ _ _ _ _) (SellInput _ _ _ _ _ _ _) = True
validTransitions (RunAuctionState _ _ _ _ _ _) (StopBiddingInput _ _ _ _ _ _ _) = True
validTransitions (BidState _ _ _ _ _ _) (BiddingInput _ _ _ _ _ _ _) = True
validTransitions (RunAuctionState _ _ _ _ _ _) (BidInput _ _ _ _ _ _ _) = True
validTransitions (InitialiseState _ _ _ _ _ _) (RunAuctionInput _ _ _ _ _ _ _) = True
validTransitions (None _) (InitialiseInput _ _ _ _ _ _ _) = True
validTransitions _ _ = False


-- | Some utility functions
fundsAtAddressCondition :: (AsContractError e) => (Value -> Bool)
    -> Address
    -> Contract () AuctionSchema e (Map.Map TxOutRef ChainIndexTxOut)
fundsAtAddressCondition condition addr = loopM go () where
    go () = do
        cur <- utxosAt addr
        sl <- currentSlot
        let presentVal = foldMap (\(_,a) -> view ciTxOutValue a) $ Map.toList cur
        if condition presentVal
            then pure (Right cur)
            else awaitSlot (sl + 1) >> pure (Left ())

contractAddress :: Ledger.Address
contractAddress = Ledger.scriptAddress (Scripts.validatorScript scriptInstance)

fundsInContract  :: AsContractError e => Contract () AuctionSchema e (Value)
fundsInContract = do
  utxos <-  utxosAt contractAddress
  return $ foldMap (view ciTxOutValue) $ map snd $ Map.toList utxos

zeroLovelace :: Value 
zeroLovelace = Ada.lovelaceValueOf 0

noKey :: PaymentPubKeyHash
noKey = stringToKey "00000000000000000000000000000000000000000000000000000000"

getContractInfo :: Contract () AuctionSchema T.Text ([(Integer,(POSIXTime,Slot))])
getContractInfo = do
    currState <- mapSMError' $ getCurrentStateSM client
    case currState of
        Just (InitialiseState _ _ _ _ _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (RunAuctionState _ _ _ _ _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (BidState _ _ _ _ _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (StopBiddingState _ _ _ _ _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (SellState _ _ _ _ _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (NotSellState _ _ _ _ _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (None triggerTimeStamps) -> pure $ (triggerTimeStamps)

getFields :: Contract () AuctionSchema T.Text (Integer, Integer, Integer, Integer, Integer, [(Integer,(POSIXTime,Slot))])
getFields = do
    currState <- mapSMError' $ getCurrentStateSM client
    case currState of
        Just (InitialiseState minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps) -> pure $ (minPrice, targetPrice, balanceA, balanceB, currentBid, triggerTimeStamps)
        Just (RunAuctionState minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps) -> pure $ (minPrice, targetPrice, balanceA, balanceB, currentBid, triggerTimeStamps)
        Just (BidState minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps) -> pure $ (minPrice, targetPrice, balanceA, balanceB, currentBid, triggerTimeStamps)
        Just (StopBiddingState minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps) -> pure $ (minPrice, targetPrice, balanceA, balanceB, currentBid, triggerTimeStamps)
        Just (SellState minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps) -> pure $ (minPrice, targetPrice, balanceA, balanceB, currentBid, triggerTimeStamps)
        Just (NotSellState minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps) -> pure $ (minPrice, targetPrice, balanceA, balanceB, currentBid, triggerTimeStamps)


-- | Beginning of Endpoint declarations
initialise :: Promise () AuctionSchema T.Text ()
initialise = endpoint @"initialise" @InitialiseParams $ \(InitialiseParams mPrice tPrice bA bB cBid) -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (InitialiseInput undefined undefined undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      mapSMError' $ initialiseSM
      oldVal <- fundsInContract
      (triggerTimeStamps) <- getContractInfo
      logic <- initialiseLogic mPrice tPrice bA bB cBid 0 0 0 0 0 triggerTimeStamps oldVal
      case logic of
        Left (minPrice, targetPrice, balanceA, balanceB, currentBid, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (InitialiseInput minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint initialise"

 
runAuction :: Promise () AuctionSchema T.Text ()
runAuction = endpoint @"runauction" @RunAuctionParams $ \(RunAuctionParams offerPriceValue) -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (RunAuctionInput undefined undefined undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (minPriceOld, targetPriceOld, balanceAOld, balanceBOld, currentBidOld, triggerTimeStamps) <- getFields
      logic <- runAuctionLogic offerPriceValue minPriceOld targetPriceOld balanceAOld balanceBOld currentBidOld triggerTimeStamps oldVal
      case logic of
        Left (minPrice, targetPrice, balanceA, balanceB, currentBid, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (RunAuctionInput minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint runAuction"

 
bid :: Promise () AuctionSchema T.Text ()
bid = endpoint @"bid" $ \() -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (BidInput undefined undefined undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (minPriceOld, targetPriceOld, balanceAOld, balanceBOld, currentBidOld, triggerTimeStamps) <- getFields
      logic <- bidLogic minPriceOld targetPriceOld balanceAOld balanceBOld currentBidOld triggerTimeStamps oldVal
      case logic of
        Left (minPrice, targetPrice, balanceA, balanceB, currentBid, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (BidInput minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint bid"

 
bidding :: Promise () AuctionSchema T.Text ()
bidding = endpoint @"bidding" $ \() -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (BiddingInput undefined undefined undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (minPriceOld, targetPriceOld, balanceAOld, balanceBOld, currentBidOld, triggerTimeStamps) <- getFields
      logic <- biddingLogic minPriceOld targetPriceOld balanceAOld balanceBOld currentBidOld triggerTimeStamps oldVal
      case logic of
        Left (minPrice, targetPrice, balanceA, balanceB, currentBid, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (BiddingInput minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint bidding"

 
stopBidding :: Promise () AuctionSchema T.Text ()
stopBidding = endpoint @"stopbidding" $ \() -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (StopBiddingInput undefined undefined undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (minPriceOld, targetPriceOld, balanceAOld, balanceBOld, currentBidOld, triggerTimeStamps) <- getFields
      logic <- stopBiddingLogic minPriceOld targetPriceOld balanceAOld balanceBOld currentBidOld triggerTimeStamps oldVal
      case logic of
        Left (minPrice, targetPrice, balanceA, balanceB, currentBid, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (StopBiddingInput minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint stopBidding"

 
sell :: Promise () AuctionSchema T.Text ()
sell = endpoint @"sell" $ \() -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (SellInput undefined undefined undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (minPriceOld, targetPriceOld, balanceAOld, balanceBOld, currentBidOld, triggerTimeStamps) <- getFields
      logic <- sellLogic minPriceOld targetPriceOld balanceAOld balanceBOld currentBidOld triggerTimeStamps oldVal
      case logic of
        Left (minPrice, targetPrice, balanceA, balanceB, currentBid, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (SellInput minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint sell"

 
notSell :: Promise () AuctionSchema T.Text ()
notSell = endpoint @"notsell" $ \() -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (NotSellInput undefined undefined undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (minPriceOld, targetPriceOld, balanceAOld, balanceBOld, currentBidOld, triggerTimeStamps) <- getFields
      logic <- notSellLogic minPriceOld targetPriceOld balanceAOld balanceBOld currentBidOld triggerTimeStamps oldVal
      case logic of
        Left (minPrice, targetPrice, balanceA, balanceB, currentBid, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (NotSellInput minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint notSell"

 

-- | Data structure used in the logic functions to track the change of the fields that may be altered from the execution of an endpoint.
data LogicOutput = LogicOutput{
    minPrice :: Integer,
    targetPrice :: Integer,
    balanceA :: Integer,
    balanceB :: Integer,
    currentBid :: Integer,
    stateVal :: Value,
    constraint :: TxConstraints AuctionInput AuctionState
}

-- | Changes the value of the minPrice field.
setMinPrice :: Integer -> LogicOutput -> LogicOutput
setMinPrice newMinPrice output = output{ minPrice = newMinPrice }

-- | Changes the value of the targetPrice field.
setTargetPrice :: Integer -> LogicOutput -> LogicOutput
setTargetPrice newTargetPrice output = output{ targetPrice = newTargetPrice }

-- | Changes the value of the balanceA field.
setBalanceA :: Integer -> LogicOutput -> LogicOutput
setBalanceA newBalanceA output = output{ balanceA = newBalanceA }

-- | Changes the value of the balanceB field.
setBalanceB :: Integer -> LogicOutput -> LogicOutput
setBalanceB newBalanceB output = output{ balanceB = newBalanceB }

-- | Changes the value of the currentBid field.
setCurrentBid :: Integer -> LogicOutput -> LogicOutput
setCurrentBid newCurrentBid output = output{ currentBid = newCurrentBid }

-- | Changes the value of the stateVal field.
setStateVal :: Value -> LogicOutput -> LogicOutput
setStateVal newStateVal output = output{ stateVal = newStateVal }

-- | Changes the value of the constraint field.
setConstraint :: TxConstraints AuctionInput AuctionState -> LogicOutput -> LogicOutput
setConstraint newConstraint output = output{ constraint = newConstraint }

getOutput :: LogicOutput -> (Integer,Integer,Integer,Integer,Integer,Value,TxConstraints AuctionInput AuctionState)
getOutput out = (minPrice out,targetPrice out,balanceA out,balanceB out,currentBid out,stateVal out,constraint out)

getSoloOutput :: LogicOutput -> (Integer,Integer,Integer,Integer,Integer,Value,TxConstraints AuctionInput AuctionState)
getSoloOutput out = (minPrice out,targetPrice out,balanceA out,balanceB out,currentBid out,stateVal out,constraint out)

-- | Logic functions (for one of the endpoints and triggers declared in the protocol
-- | TODO: customize this section of the contract!

initialiseLogic :: Integer ->
                   Integer ->
                   Integer ->
                   Integer ->
                   Integer ->
                   Integer ->
                   Integer ->
                   Integer ->
                   Integer ->
                   Integer ->
                   [(Integer, (POSIXTime, Slot))] ->
                   Value ->
                   Contract ()
                            AuctionSchema
                            T.Text
                            (Either (Integer,
                                     Integer,
                                     Integer,
                                     Integer,
                                     Integer,
                                     Value,
                                     TxConstraints AuctionInput AuctionState)
                                    AuctionError)
initialiseLogic mPrice tPrice bA bB cBid minPrice targetPrice balanceA balanceB currentBid triggerTimeStamps stateVal = do returnOutputOk $ setMinPrice mPrice $ setTargetPrice tPrice $ setBalanceA bA $ setBalanceB bB $ setCurrentBid cBid $ setStateVal stateVal output
                    where output :: LogicOutput
                          output = LogicOutput minPrice targetPrice balanceA balanceB currentBid stateVal mempty
                          printMsg :: String -> Contract () AuctionSchema T.Text ()
                          printMsg msg = logInfo @String msg
                          printError :: String -> Contract () AuctionSchema T.Text ()
                          printError err = logError @String err
                          returnError :: String ->
                                         Contract ()
                                                  AuctionSchema
                                                  T.Text
                                                  (Either (Integer,
                                                           Integer,
                                                           Integer,
                                                           Integer,
                                                           Integer,
                                                           Value,
                                                           TxConstraints AuctionInput AuctionState)
                                                          AuctionError)
                          returnError err = pure $ Right $ Error $ T.pack err
                          returnOk :: Contract ()
                                               AuctionSchema
                                               T.Text
                                               (Either (Integer,
                                                        Integer,
                                                        Integer,
                                                        Integer,
                                                        Integer,
                                                        Value,
                                                        TxConstraints AuctionInput AuctionState)
                                                       AuctionError)
                          returnOk = pure $ Left (minPrice,
                                                  targetPrice,
                                                  balanceA,
                                                  balanceB,
                                                  currentBid,
                                                  stateVal,
                                                  mempty)
                          returnOutputOk :: LogicOutput ->
                                            Contract ()
                                                     AuctionSchema
                                                     T.Text
                                                     (Either (Integer,
                                                              Integer,
                                                              Integer,
                                                              Integer,
                                                              Integer,
                                                              Value,
                                                              TxConstraints AuctionInput
                                                                            AuctionState)
                                                             AuctionError)
                          returnOutputOk out = pure $ Left $ getOutput out
                          returnOkWith :: Integer ->
                                          Integer ->
                                          Integer ->
                                          Integer ->
                                          Integer ->
                                          Value ->
                                          TxConstraints AuctionInput AuctionState ->
                                          Contract ()
                             