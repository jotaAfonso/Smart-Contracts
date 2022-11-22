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
data PingPongRecState =
    None [(Integer,(POSIXTime,Slot))]
    | InitialiseState [(Integer,(POSIXTime,Slot))]
    | PingState [(Integer,(POSIXTime,Slot))]
    deriving stock (Show, Generic)

-- | Declaration of the inputs that will be used for the transitions of the State Machine
data PingPongRecInput =
    InitialiseInput [(Integer,(POSIXTime,Slot))] Value
    | PingInput [(Integer,(POSIXTime,Slot))] Value
    | PongInput [(Integer,(POSIXTime,Slot))] Value
    deriving stock (Show, Generic)

-- | Make the types possible to use in the "on chain" part of Plutus (State Machine)

PlutusTx.makeLift ''PingPongRecState
PlutusTx.makeLift ''PingPongRecInput
PlutusTx.unstableMakeIsData ''PingPongRecState
PlutusTx.unstableMakeIsData ''PingPongRecInput
-- | Declaration of the errors that will be used throughout this contract
data PingPongRecError = Error Text
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''PingPongRecError


-- | Transforming errors into Text to comply with endpoint definitions
mapSMError' :: Contract w s SM.SMContractError a -> Contract w s Text a
mapSMError' = mapError $ T.pack . show

mapContractError' :: Contract w s ContractError a -> Contract w s Text a
mapContractError' = mapError $ T.pack . show

-- | Contract schema: Endpoint and the parameters they receive
type PingPongRecSchema =
        Endpoint "initialise" ()
        .\/ Endpoint "ping" ()
        .\/ Endpoint "pong" ()




stringToKey :: String -> PaymentPubKeyHash
stringToKey key = PaymentPubKeyHash{unPaymentPubKeyHash = fromString key}

{-# INLINABLE validateKeys #-}
validateKeys :: [PaymentPubKeyHash] -> TxConstraints Void Void
validateKeys keys = if null keys
    then mempty
    else Constraints.mustSatisfyAnyOf $ map (Constraints.mustBeSignedBy) keys

-- | State Machine transactions and client definition
{-# INLINABLE transition #-}
transition :: State PingPongRecState -> PingPongRecInput -> Maybe (TxConstraints Void Void, State PingPongRecState)
transition State{stateData=oldData,stateValue} input = case (oldData, input) of
    (None _, InitialiseInput triggerTimeStamps stateVal) -> Just(mempty, State{stateData = InitialiseState triggerTimeStamps, stateValue = stateVal})
    (InitialiseState _, PingInput triggerTimeStamps stateVal) -> Just(mempty, State{stateData = PingState triggerTimeStamps, stateValue = stateVal})
    (PingState _, PongInput triggerTimeStamps stateVal) -> Just(mempty, State{stateData = InitialiseState triggerTimeStamps, stateValue = stateVal})
    
    
    _ -> Nothing

{-# INLINABLE transitionCheck #-}
transitionCheck :: PingPongRecState -> PingPongRecInput -> ScriptContext -> Bool
transitionCheck state input context = case (state, input) of
    (None triggerTimeStamps, InitialiseInput _ _) -> True
    (InitialiseState triggerTimeStamps, PingInput _ _) -> True
    (PingState triggerTimeStamps, PongInput _ _) -> True
    
    
    _ -> False
    where
        checkKeys keys = any (txSignedBy $ (scriptContextTxInfo context)) $ map unPaymentPubKeyHash keys

{-# INLINABLE machine #-}
machine :: SM.StateMachine PingPongRecState PingPongRecInput
machine = SM.StateMachine
        { SM.smTransition = transition
        , SM.smFinal = isFinal
        , SM.smCheck = transitionCheck
        , SM.smThreadToken = Nothing
        }
    where
        
        isFinal _       = False

{-# INLINABLE mkValidator #-} 
mkValidator :: Scripts.ValidatorType (SM.StateMachine PingPongRecState PingPongRecInput)
mkValidator = SM.mkValidator machine

scriptInstance :: Scripts.TypedValidator (SM.StateMachine PingPongRecState PingPongRecInput)
scriptInstance = Scripts.mkTypedValidator @(SM.StateMachine PingPongRecState PingPongRecInput)
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @PingPongRecState @PingPongRecInput

machineInstance :: SM.StateMachineInstance PingPongRecState PingPongRecInput
machineInstance = SM.StateMachineInstance machine scriptInstance

client :: SM.StateMachineClient PingPongRecState PingPongRecInput
client = SM.mkStateMachineClient machineInstance

-- | Methods to interact with the state machine

runContractStep :: SM.StateMachineClient PingPongRecState PingPongRecInput -> PingPongRecInput -> TxConstraints PingPongRecInput PingPongRecState -> Contract () PingPongRecSchema SM.SMContractError ( Maybe PingPongRecState )
runContractStep client input constraint = 
    do 
    pkh <- ownPaymentPubKeyHash
    r <- SM.runStepWith (Constraints.ownPaymentPubKeyHash pkh) constraint client input 
    case r of 
      SM.TransitionFailure i -> pure Nothing
      SM.TransitionSuccess s -> pure (Just s)

initialiseSM :: Contract () PingPongRecSchema SM.SMContractError (Maybe PingPongRecState)
initialiseSM = do
    currentState <- getCurrentStateSM client
    case currentState of
        Nothing -> do
          let triggerTimeStamps = []
          SM.runInitialise client (None  triggerTimeStamps) mempty
          pure Nothing
        x -> pure x

getCurrentStateSM :: SM.StateMachineClient PingPongRecState PingPongRecInput -> Contract () PingPongRecSchema SM.SMContractError (Maybe PingPongRecState)
getCurrentStateSM client = do
  currentState <- SM.getOnChainState client
  case currentState of
    Just (SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=state}}, _) -> pure (Just (state))
    _ -> pure Nothing

isValidCallInState :: Maybe PingPongRecState -> PingPongRecInput -> Bool
isValidCallInState Nothing input = canInitialiseSM input
isValidCallInState (Just state) input = validTransitions state input

canInitialiseSM :: PingPongRecInput -> Bool
canInitialiseSM (InitialiseInput _ _) = True
canInitialiseSM _ = False

validTransitions :: PingPongRecState -> PingPongRecInput -> Bool
validTransitions (PingState _) (PongInput _ _) = True
validTransitions (InitialiseState _) (PingInput _ _) = True
validTransitions (None _) (InitialiseInput _ _) = True
validTransitions _ _ = False


-- | Some utility functions
fundsAtAddressCondition :: (AsContractError e) => (Value -> Bool)
    -> Address
    -> Contract () PingPongRecSchema e (Map.Map TxOutRef ChainIndexTxOut)
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

fundsInContract  :: AsContractError e => Contract () PingPongRecSchema e (Value)
fundsInContract = do
  utxos <-  utxosAt contractAddress
  return $ foldMap (view ciTxOutValue) $ map snd $ Map.toList utxos

zeroLovelace :: Value 
zeroLovelace = Ada.lovelaceValueOf 0

noKey :: PaymentPubKeyHash
noKey = stringToKey "00000000000000000000000000000000000000000000000000000000"

getContractInfo :: Contract () PingPongRecSchema T.Text ([(Integer,(POSIXTime,Slot))])
getContractInfo = do
    currState <- mapSMError' $ getCurrentStateSM client
    case currState of
        Just (InitialiseState triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (PingState triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (None triggerTimeStamps) -> pure $ (triggerTimeStamps)

getFields :: Contract () PingPongRecSchema T.Text ([(Integer,(POSIXTime,Slot))])
getFields = do
    currState <- mapSMError' $ getCurrentStateSM client
    case currState of
        Just (InitialiseState triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (PingState triggerTimeStamps) -> pure $ (triggerTimeStamps)


-- | Beginning of Endpoint declarations
initialise :: Promise () PingPongRecSchema T.Text ()
initialise = endpoint @"initialise" $ \() -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (InitialiseInput undefined undefined) then do
    -- | Calling business logic function...
      mapSMError' $ initialiseSM
      oldVal <- fundsInContract
      (triggerTimeStamps) <- getContractInfo
      logic <- initialiseLogic triggerTimeStamps oldVal
      case logic of
        Left (stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (InitialiseInput triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint initialise"

 
ping :: Promise () PingPongRecSchema T.Text ()
ping = endpoint @"ping" $ \() -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (PingInput undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (triggerTimeStamps) <- getFields
      logic <- pingLogic triggerTimeStamps oldVal
      case logic of
        Left (stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (PingInput triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint ping"

 
pong :: Promise () PingPongRecSchema T.Text ()
pong = endpoint @"pong" $ \() -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (PongInput undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (triggerTimeStamps) <- getFields
      logic <- pongLogic triggerTimeStamps oldVal
      case logic of
        Left (stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (PongInput triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint pong"

 

-- | Data structure used in the logic functions to track the change of the fields that may be altered from the execution of an endpoint.
data LogicOutput = LogicOutput{
    stateVal :: Value,
    constraint :: TxConstraints PingPongRecInput PingPongRecState
}

-- | Changes the value of the stateVal field.
setStateVal :: Value -> LogicOutput -> LogicOutput
setStateVal newStateVal output = output{ stateVal = newStateVal }

-- | Changes the value of the constraint field.
setConstraint :: TxConstraints PingPongRecInput PingPongRecState -> LogicOutput -> LogicOutput
setConstraint newConstraint output = output{ constraint = newConstraint }

getOutput :: LogicOutput -> (Value,TxConstraints PingPongRecInput PingPongRecState)
getOutput out = (stateVal out,constraint out)

getSoloOutput :: LogicOutput -> (Value,TxConstraints PingPongRecInput PingPongRecState)
getSoloOutput out = (stateVal out,constraint out)

-- | Logic functions (for one of the endpoints and triggers declared in the protocol
-- | TODO: customize this section of the contract!

initialiseLogic :: [(Integer,(POSIXTime,Slot))] -> Value -> Contract () PingPongRecSchema T.Text (Either (Value, TxConstraints PingPongRecInput PingPongRecState) PingPongRecError)
initialiseLogic triggerTimeStamps stateVal =  do
  -- TODO: Implement the logic here
  returnError "undefined function"
    where
        -- | Data structure that stores the information to eventually update the information inside the current or new state
        output :: LogicOutput
        output = LogicOutput stateVal mempty

        -- | Prints a message in the console
        printMsg :: String -> Contract () PingPongRecSchema T.Text ()
        printMsg msg = logInfo @String msg

        -- | Prints an error in the console
        printError :: String -> Contract () PingPongRecSchema T.Text ()
        printError err = logError @String err

        -- | Returns an error
        returnError :: String -> Contract () PingPongRecSchema T.Text (Either (Value, TxConstraints PingPongRecInput PingPongRecState) PingPongRecError)
        returnError err = pure $ Right $ Error $ T.pack err

        -- | Returns in case of sucess with the fields unaltered
        returnOk :: Contract () PingPongRecSchema T.Text (Either (Value, TxConstraints PingPongRecInput PingPongRecState) PingPongRecError)
        returnOk = pure $ Left (stateVal, mempty)

        -- | Returns in case of sucess with the help of a data structure that tracks the change of some fields
        returnOutputOk :: LogicOutput -> Contract () PingPongRecSchema T.Text (Either (Value, TxConstraints PingPongRecInput PingPongRecState) PingPongRecError)
        returnOutputOk out = pure $ Left $ getOutput out

        -- | Returns in case of sucess with the all the fields especified with the same or a new value
        returnOkWith :: Value -> TxConstraints PingPongRecInput PingPongRecState -> Contract () PingPongRecSchema T.Text (Either (Value, TxConstraints PingPongRecInput PingPongRecState) PingPongRecError)
        returnOkWith stateValRet constraintRet = pure $ Left (stateValRet, constraintRet)

pingLogic :: [(Integer,(POSIXTime,Slot))] -> Value -> Contract () PingPongRecSchema T.Text (Either (Value, TxConstraints PingPongRecInput PingPongRecState) PingPongRecError)
pingLogic triggerTimeStamps stateVal =  do
  -- TODO: Implement the logic here
  returnError "undefined function"
    where
        -- | Data structure that stores the information to eventually update the information inside the current or new state
        output :: LogicOutput
        output = LogicOutput stateVal mempty

        -- | Prints a message in the console
        printMsg :: String -> Contract () PingPongRecSchema T.Text ()
        printMsg msg = logInfo @String msg

        -- | Prints an error in the console
        printError :: String -> Contract () PingPongRecSchema T.Text ()
        printError err = logError @String err

        -- | Returns an error
        returnError :: String -> Contract () PingPongRecSchema T.Text (Either (Value, TxConstraints PingPongRecInput PingPongRecState) PingPongRecError)
        returnError err = pure $ Right $ Error $ T.pack err

        -- | Returns in case of sucess with the fields unaltered
        returnOk :: Contract () PingPongRecSchema T.Text (Either (Value, TxConstraints PingPongRecInput PingPongRecState) PingPongRecError)
        returnOk = pure $ Left (stateVal, mempty)

        -- | Returns in case of sucess with the help of a data structure that tracks the change of some fields
        returnOutputOk :: LogicOutput -> Contract () PingPongRecSchema T.Text (Either (Value, TxConstraints PingPongRecInput PingPongRecState) PingPongRecError)
        returnOutputOk out = pure $ Left $ getOutput out

        -- | Returns in case of sucess with the all the fields especified with the same or a new value
        returnOkWith :: Value -> TxConstraints PingPongRecInput PingPongRecState -> Contract () PingPongRecSchema T.Text (Either (Value, TxConstraints PingPongRecInput PingPongRecState) PingPongRecError)
        returnOkWith stateValRet constraintRet = pure $ Left (stateValRet, constraintRet)

pongLogic :: [(Integer,(POSIXTime,Slot))] -> Value -> Contract () PingPongRecSchema T.Text (Either (Value, TxConstraints PingPongRecInput PingPongRecState) PingPongRecError)
pongLogic triggerTimeStamps stateVal =  do
  -- TODO: Implement the logic here
  returnError "undefined function"
    where
        -- | Data structure that stores the information to eventually update the information inside the current or new state
        output :: LogicOutput
        output = LogicOutput stateVal mempty

        -- | Prints a message in the console
        printMsg :: String -> Contract () PingPongRecSchema T.Text ()
        printMsg msg = logInfo @String msg

        -- | Prints an error in the console
        printError :: String -> Contract () PingPongRecSchema T.Text ()
        printError err = logError @String err

        -- | Returns an error
        returnError :: String -> Contract () PingPongRecSchema T.Text (Either (Value, TxConstraints PingPongRecInput PingPongRecState) PingPongRecError)
        returnError err = pure $ Right $ Error $ T.pack err

        -- | Returns in case of sucess with the fields unaltered
        returnOk :: Contract () PingPongRecSchema T.Text (Either (Value, TxConstraints PingPongRecInput PingPongRecState) PingPongRecError)
        returnOk = pure $ Left (stateVal, mempty)

        -- | Returns in case of sucess with the help of a data structure that tracks the change of some fields
        returnOutputOk :: LogicOutput -> Contract () PingPongRecSchema T.Text (Either (Value, TxConstraints PingPongRecInput PingPongRecState) PingPongRecError)
        returnOutputOk out = pure $ Left $ getOutput out

        -- | Returns in case of sucess with the all the fields especified with the same or a new value
        returnOkWith :: Value -> TxConstraints PingPongRecInput PingPongRecState -> Contract () PingPongRecSchema T.Text (Either (Value, TxConstraints PingPongRecInput PingPongRecState) PingPongRecError)
        returnOkWith stateValRet constraintRet = pure $ Left (stateValRet, constraintRet)





-- | Defining the contract to run on the playground
contract :: Contract () PingPongRecSchema T.Text ()
contract = selectList[initialise, ping, pong]

endpoints :: Contract () PingPongRecSchema T.Text ()
endpoints = forever contract

mkSchemaDefinitions ''PingPongRecSchema