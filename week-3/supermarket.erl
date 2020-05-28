-module(supermarket).
-export(
    [ db_from_list/1
    , order_total/1
    , scan_barcodes/1
    , prepare_bill/2
    , print_bill/1
    ]).

-type db() :: none | { product(), db(), db() }.
-type product() :: 
    { Barcode :: barcode()
    , Name :: string()
    , Price :: [price()]
    }.
-type barcode() :: pos_integer(). 
-type price() :: unavailable | pos_integer().
-type order() :: [{barcode(), Quantity::pos_integer()}].
-type bill_format() :: 
    { Total :: string()
    , [ { Name :: string()
        , Quantity :: string()
        , Price :: string()
        , Amount :: string()
        } ] 
    }.

-spec db_from_list([product()]) -> db().
db_from_list(_) -> todo.

-spec order_total(order()) -> pos_integer().
order_total(_) -> todo.

-spec scan_barcodes([barcode()]) -> order().
scan_barcodes(_) -> todo.

-spec prepare_bill(db(), order()) -> bill_format().
prepare_bill(_,_) -> todo.

-spec print_bill(bill_format()) -> string().
print_bill(_) -> todo.