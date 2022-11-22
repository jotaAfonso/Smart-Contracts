# @version ^0.3.5

# import PingPongGame_Player as Player
interface Starter:
    def StartPingPong(pingPongTimesValue: int128): nonpayable
    def Pong(currentPingPongTimes: int128): nonpayable
    def FinishGame(): nonpayable

enum StateType:
    PingpongPlayerCreated
    PingPonging
    GameFinished
    
event PingPongEvent:
    LogDetails: indexed(String[100])
    
PingPongGameName: public(String[100])
State: public(StateType)
GameStarter: public(address)

@external
def __init__(pingPongGameName: String[100]):
    self.GameStarter = msg.sender
    self.PingPongGameName = pingPongGameName
    self.State = StateType.PingpongPlayerCreated
   
@external
def Ping(currentPingPongTimes : int128):
    remainingPingPongTimes: int128 = currentPingPongTimes - 1
    starter: Starter = Starter(msg.sender)
    if remainingPingPongTimes > 0:
        self.State = StateType.PingPonging
        log PingPongEvent("Inside_Contract_2")
        starter.Pong(remainingPingPongTimes)
    else:
        log PingPongEvent("Inside_Contract_2")
        self.State = StateType.GameFinished
        starter.FinishGame()

@external
def FinishGame():
    self.State = StateType.GameFinished
    