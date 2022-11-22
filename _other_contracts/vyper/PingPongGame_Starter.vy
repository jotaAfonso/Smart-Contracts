# @version ^0.3.5

# import PingPongGame_Player as Player
interface Player:
    def __init__(gameName: String[100]): nonpayable
    def Ping(currentPingPongTimes: int128): nonpayable
    def FinishGame(): nonpayable

enum StateType:
    GAMEPROVISIONED
    PINGPONGING
    GAMEFINISHED
    
event PingPongEvent:
    LogDetails: indexed(String[100])
    
pingPongGameName: public(String[100])
state: public(StateType)
gameStarter: public(address)
pingPongTimes: public(int128)
gamePlayer: public(Player)

@external
def __init__(gameName: String[100], playerAddress: address):
    self.gameStarter = msg.sender
    self.pingPongGameName = gameName
    self.gamePlayer = Player(playerAddress)
    self.gamePlayer.__init__(gameName)
    self.state = StateType.GAMEPROVISIONED

@external
def StartPingPong(pingPongTimesValue: int128):
    self.pingPongTimes = pingPongTimesValue
    self.state = StateType.PINGPONGING
    self.gamePlayer.Ping(pingPongTimesValue)

@external
def Pong(currentPingPongTimes: int128):
    remainingPingPongTimes: int128 = currentPingPongTimes - 1
    if remainingPingPongTimes > 0:
        self.state = StateType.PINGPONGING
        log PingPongEvent("Inside_Contract_1")
        self.gamePlayer.Ping(remainingPingPongTimes)
    else:
        log PingPongEvent("Inside_Contract_1")
        self.state = StateType.GAMEFINISHED
        self.gamePlayer.FinishGame()

@external
def FinishGame():
    self.state = StateType.GAMEFINISHED
    