module Genaux

open Expecto
open FsCheck

type 'a ListOfAtLeast2 = ListOfAtLeast2 of 'a list

//let addToConfig config = 
//  { config with arbitrary = typeof<ListOfAtLeast2>.DeclaringType::config.arbitrary}


