// functions on arbitrary lists

// get all but the first item in a list
function cdr(vec) = 
    vec == [] ? [] : 
    [for(i=[1:1:len(vec)-1]) vec[i]];

// reverse a list
function reverse(vec) = 
    len(vec) == 0 ? vec :
    [each reverse(cdr(vec)), vec[0]];

// sum the elements of a list
function sum(vec, acc=0) = 
    is_list(vec[0]) && acc == 0 ? sum(vec, [for(i = [1:1:len(vec[0])]) 0]) :
    vec == [] ? acc :
    sum(cdr(vec), acc + vec[0]);

// mean of a list
function avg(vec) = sum(vec) / len(vec);

// and of a list of booleans
function and(vec) = 
    vec == [] ? true : 
    !is_bool(vec[0]) ? false :
    vec[0] ? and(cdr(vec)) : 
    false;

// or of a list of booleans
function or(vec) = 
    vec == [] ? false :
    !is_bool(vec[0]) ? false :
    vec[0] ? true :
    or(cdr(vec));

// remove all of an item from a list by value
function remove(list, bad) = (
    list == [] ? list :
    list[0] == bad ? remove(cdr(list), bad) :
    [list[0], each remove(cdr(list), bad)]
);

// return all items in list for which f returns true
function filter(list, f) = (
    list == [] ? list :
    !f(list[0]) ? filter(cdr(list)) :
    [list[0], each filter(cdr(list), bad)]
);

// find the highest something in a list using the provided comparator
function select(list, comp, highest) = (
    list == [] ? highest :
    comp(highest, list[0]) > 0 ? select(cdr(list), comp, highest) :
    select(cdr(list), comp, list[0])
);
// selection sort a list using a comparator (which should return a number for how the first arg sorts to the second)
function sort(list, comp, acc=[]) = (
    list == [] ? acc :
    sort(remove(list, select(list, comp, undef)), comp, [select(list, comp, undef), each acc])
);

// tell whether a list contains all elements of another list
function contains(L, elts) = (
    len(elts) == 0 ? true :
    len(L) == 0 ? false :
    len(elts) == 1 ? 
        L[0] == elts[0] ? true : contains(cdr(L), elts)
    : contains(L, [elts[0]]) && contains(L, cdr(elts))
);

// join two lists, taking only one copy of any duplicates
function merge(L1, L2, acc=[]) = 
    L1 == [] && L2 == [] ? acc :
    L1 == [] ? [each acc, each L2] :
    L2 == [] ? [each acc, each L1] :
    contains(L2, [L1[0]]) ? merge(cdr(L1), remove(L2, L1[0]), [each acc, L1[0]]) :
    merge(cdr(L1), L2, [each acc, L1[0]]);

// find all the indices where x appears in L
function index(L, x, acc=[0], margin=0) = (
    len(L) == 0 ? cdr(acc) : // trim off the counter
    distance(L[0], x) <= margin ? index(cdr(L), x, concat(acc[0]+1, acc)) : // save the current index
    index(cdr(L), x, concat(acc[0]+1, cdr(acc))) // increase the counter
);

