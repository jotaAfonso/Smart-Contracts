module 0x1::error { 
 
  /// Caller specified an invalid argument (http: 400) 
  const INVALID_ARGUMENT: u64 = 0x1; 
 
  /// An input or result of a computation is out of range (http: 400) 
  const OUT_OF_RANGE: u64 = 0x2; 
 
  /// The system is not in a state where the operation can be performed (http: 400) 
  const INVALID_STATE: u64 = 0x3; 
 
  /// Request not authenticated due to missing, invalid, or expired auth token (http: 401) 
  const UNAUTHENTICATED: u64 = 0x4; 
 
  /// client does not have sufficient permission (http: 403) 
  const PERMISSION_DENIED: u64 = 0x5; 
 
  /// A specified resource is not found (http: 404) 
  const NOT_FOUND: u64 = 0x6; 
 
  /// Concurrency conflict, such as read-modify-write conflict (http: 409) 
  const ABORTED: u64 = 0x7; 
 
  /// The resource that a client tried to create already exists (http: 409) 
  const ALREADY_EXISTS: u64 = 0x8; 
 
  /// Out of gas or other forms of quota (http: 429) 
  const RESOURCE_EXHAUSTED: u64 = 0x9; 
 
  /// Request cancelled by the client (http: 499) 
  const CANCELLED: u64 = 0xA; 
 
  /// Internal error (http: 500) 
  const INTERNAL: u64 = 0xB; 
 
  /// Feature not implemented (http: 501) 
  const NOT_IMPLEMENTED: u64 = 0xC; 
 
  /// The service is currently unavailable. Indicates that a retry could solve the issue (http: 503) 
  const UNAVAILABLE: u64 = 0xD; 
 
  /// Construct a canonical error code from a category and a reason. 
  public fun canonical(category: u64, reason: u64): u64 { 
    (category << 16) + reason 
  } 
  spec canonical { 
    pragma opaque = true; 
    // TODO: `<<` has different meanings in code and spec in case of overvlow. 
    let shl_res = (category * 65536) % 18446744073709551616; // (category << 16) 
    ensures [concrete] result == shl_res + reason; 
    aborts_if [abstract] false; 
    ensures [abstract] result == category; 
  } 
 
  /// Functions to construct a canonical error code of the given category. 
  public fun invalid_argument(r: u64): u64 {  canonical(INVALID_ARGUMENT, r) } 
  public fun out_of_range(r: u64): u64 {  canonical(OUT_OF_RANGE, r) } 
  public fun invalid_state(r: u64): u64 {  canonical(INVALID_STATE, r) } 
  public fun unauthenticated(r: u64): u64 { canonical(UNAUTHENTICATED, r) } 
  public fun permission_denied(r: u64): u64 { canonical(PERMISSION_DENIED, r) } 
  public fun not_found(r: u64): u64 { canonical(NOT_FOUND, r) } 
  public fun aborted(r: u64): u64 { canonical(ABORTED, r) } 
  public fun already_exists(r: u64): u64 { canonical(ALREADY_EXISTS, r) } 
  public fun resource_exhausted(r: u64): u64 {  canonical(RESOURCE_EXHAUSTED, r) } 
  public fun internal(r: u64): u64 {  canonical(INTERNAL, r) } 
  public fun not_implemented(r: u64): u64 {  canonical(NOT_IMPLEMENTED, r) } 
  public fun unavailable(r: u64): u64 { canonical(UNAVAILABLE, r) } 
}