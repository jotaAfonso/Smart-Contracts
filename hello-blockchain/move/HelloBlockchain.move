module 0x1::HelloBlockchain {
  use sui::object::{Self, UID};
  use sui::tx_context::TxContext;

  // Use this dependency to get a type wrapper for UTF-8 strings
  use std::string::{Self, String};

  /// A dummy Object that holds a String type
  struct Message {
      id: UID,

      /// Here it is - the String type
      name: String
  }

  /// Create a message Object by passing raw bytes
  public fun init(
      message_bytes: vector<u8>, ctx: &mut TxContext
  ): Message {
      Message {
          id: object::new(ctx),
          name: string::utf8(message_bytes)
      }
  }

  // TODO: search use of states
  public fun SendRequest(
      message_bytes: vector<u8>, ctx: &mut TxContext
  ):  Message {
    Message {
          id: object::new(ctx),
          name: string::utf8(message_bytes)
      }
  }

  public fun SendRequest(
      message_bytes: vector<u8>, ctx: &mut TxContext
  ):  Message {
    Message {
          id: object::new(ctx),
          name: string::utf8(message_bytes)
      }
  }
}