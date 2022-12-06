 
module 0x1::itemList {
    use 0x1::error; 
    use Std::Signer;

    //
    // Errors
    //

    /// Listing zero token
    const ELISTING_ZERO_TOKEN: u64 = 5;

    struct Listing has key, drop, store {
        price: u64,
        name: vector<u8>,
    }

    //
    // public functions
    //

    /// Return a listing struct, marketplace owner can use this function to create a listing and store it in its inventory
    public fun create_listing(
        price_Param: u64,
        name_Param: vector<u8>,
    ): Listing {
        assert!(price_Param > 0, error::invalid_argument(ELISTING_ZERO_TOKEN));
        let localItem = Listing {
            price: price_Param,
            name: name_Param
        };

        localItem
    }

    public fun create_Item(
        owner: &signer,
        price: u64,
        name: vector<u8>,
    ) {
        let item = create_listing(
            price,
            name,
        );
       
        // add a new record to the listing
        move_to<Listing>(owner,item)
    }
}