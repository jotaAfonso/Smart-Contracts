pragma solidity ^0.5.0;
contract Auction {

    event AuctionStoredEvent(address buyer, int price);

    enum StateType {Started, Bidding, Sold, NotSold}
    StateType public State;

    address buyer_1;
    address buyer_2;
    int balanceBuyer_1;
    int balanceBuyer_2;
    
    address seller;
    int minimumPrice;
    int targetPrice;
    
    int currentBid;
    address currentBidder;

    function Auction(address buyerFirst, address buyerSecond, int balanceFirst, int balanceSecond, int minPrice, int objectivePrice) public {
        seller = msg.sender;
        buyer_1 = buyerFirst;
        buyer_2 = buyerSecond;
        balanceBuyer_1 = balanceFirst;
        balanceBuyer_2 = balanceSecond;
        minimumPrice = minPrice;
        targetPrice = objectivePrice;
        currentBid = 0;
        State = StateType.Started;
        currentBidder = seller;
    }
    
    function runAuction() {
        bool result = true; 
        while (currentBid < targetPrice) {
            if (currentBidder != buyer_1)
                result = bid(buyer_1);
            else 
                result = bid(buyer_1);
            if (!result)
                break;
        }
        if(currentBid < minimumPrice){
            currentBidder = seller;
            State = State.NotSold;
        }
        else{
            State = State.Sold;
        }
    } 

    function bid(address buyer) public returns (bool) {
        if (buyer_1 != buyer && buyer_2 != buyer) {
            return;
        }
        if (currentBidder == msg.sender) {
            return;
        }
        if (getBalance(buyer) <= currentBid) {
            return false;
        }
        else{
            currentBid = currentBid + 1;
            currentBidder = buyer;
            State = StateType.Bidding;
            return true;
        }
    }

    function getBalance(address buyer) private returns (int) {
        if (buyer == buyer_1)
            return balanceBuyer_1;
        else
            return balanceBuyer_2;
    }
}