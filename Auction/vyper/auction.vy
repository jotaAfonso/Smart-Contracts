# @version ^0.3.5

enum StateType:
    Started
    Bidding
    Sold
    NotSold

event AuctionStoredEvent:
    buyer: address
    price: int128

state: public(StateType)

buyer_1: public(address)
buyer_2: public(address)
balanceBuyer_1: public(int128)
balanceBuyer_2: public(int128)

seller: public(address)
minimumPrice: public(int128)
targetPrice: public(int128)

currentBid: public(int128)
currentBidder: public(address)

@external
def __init__(buyerFirst: address, buyerSecond: address, balanceFirst: int128, balanceSecond: int128, minPrice: int128, objectivePrice: int128):
    self.seller = msg.sender
    self.buyer_1 = buyerFirst
    self.buyer_2 = buyerSecond
    self.balanceBuyer_1 = balanceFirst
    self.balanceBuyer_2 = balanceSecond
    self.minimumPrice = minPrice
    self.targetPrice = objectivePrice
    self.currentBid = 0
    self.state = StateType.Started
    self.currentBidder = self.seller

@external
def runAuction():
    assert self.state == StateType.Started, "Invalid State"
    result: bool = True
    start: int128 = 0
    for i in range(start, start + 10):
        if self.currentBidder != self.buyer_1:
            result = self.bid(self.buyer_1)
        else :
            result = self.bid(self.buyer_2)
        if result == False:
            break
    if self.currentBid < self.minimumPrice:
        self.currentBidder = self.seller
        self.state = StateType.NotSold
    else:
        self.state = StateType.Sold

@internal
def bid(buyer: address) -> bool:
    assert self.buyer_1 == buyer or self.buyer_2 == buyer, "Invalid Buyer"
    assert self.currentBidder != msg.sender, "Invalid Bidder"            
    if self.getBalance(buyer) <= self.currentBid:
        return False
    else:
        self.currentBid = self.currentBid + 1
        self.currentBidder = buyer
        self.state = StateType.Bidding
        return True

@internal
def getBalance(buyer: address) -> int128:
        if buyer == self.buyer_1:
            return self.balanceBuyer_1
        else:
            return self.balanceBuyer_2