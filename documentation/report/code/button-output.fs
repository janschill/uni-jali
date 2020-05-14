val it : AbstractSyntax.Value =
  ADTValue
    ("Tag","Node",
     [StringValue "div";
      ListValue [TupleValue (StringValue "class",StringValue "body")];
      ListValue
        [ADTValue
           ("Tag","Node",
            [StringValue "button";
             ListValue [TupleValue (StringValue "onClick",IntegerValue 1)];
             ListValue [ADTValue ("Text","Node",[StringValue "Increment"])]]);
         ADTValue ("Text","Node",[IntegerValue 0]);
         ADTValue
           ("Tag","Node",
            [StringValue "button";
             ListValue [TupleValue (StringValue "onClick",IntegerValue -1)];
             ListValue [ADTValue ("Text","Node",[StringValue "Decrement"])]])]])