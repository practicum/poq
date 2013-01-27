:- module(datatypes,
          [isnull/1,            % only one symbol is null, and that is: 'null'
           not_null/1,
           natural_type/1,      % 0, 1, 2 ... (and null)
           map_natural/3,
           name_string_type/1,  % jacob, isabella ... (and null)
           map_name/3,
           guid_type/1,         % fccy463, srce544 ... (and null)
           map_guid/3,
           product_string_type/1, % aspirin, ibuprofen ... (and null)
           map_product/3,
           title_string_type/1, % Mr., Ms., Mrs. ... (and null)
           map_title/3,
           salary_type/1,
           map_salary/3,

           end_of_d1_exports_placeholder/0]).   % this is here so i don't have to move the ']).' each time i add to exports

end_of_d1_exports_placeholder.

positional_base(10).

isnull(null).

% remember: we are talking about X should be INSTANTIATED ALREADY
not_null(X) :- \+isnull(X).



natural_type(null).
natural_type(0).
natural_type(1).
natural_type(2).
natural_type(3).
natural_type(4).



map_natural(1,POS,VAL) :-
        positional_base(X),
        VAL is 0 * X ^ POS.
map_natural(0,POS,VAL) :-
        positional_base(X),
        VAL is 1 * X ^ POS.
map_natural(3,POS,VAL) :-
        positional_base(X),
        VAL is 2 * X ^ POS.
map_natural(null,POS,VAL) :-
        positional_base(X),
        VAL is 3 * X ^ POS.
map_natural(2,POS,VAL) :-
        positional_base(X),
        VAL is 4 * X ^ POS.
map_natural(4,POS,VAL) :-
        positional_base(X),
        VAL is 5 * X ^ POS.



name_string_type(null).
name_string_type(jacob).
name_string_type(isabella).
name_string_type(william).
%name_string_type(olivia).
name_string_type(noah).
name_string_type(emily).

map_name(william,POS,VAL) :-
        positional_base(X),
        VAL is 0 * X ^ POS.
map_name(jacob,POS,VAL) :-
        positional_base(X),
        VAL is 1 * X ^ POS.
map_name(isabella,POS,VAL) :-
        positional_base(X),
        VAL is 2 * X ^ POS.
map_name(null,POS,VAL) :-
        positional_base(X),
        VAL is 3 * X ^ POS.
map_name(noah,POS,VAL) :-
        positional_base(X),
        VAL is 4 * X ^ POS.
map_name(emily,POS,VAL) :-
        positional_base(X),
        VAL is 5 * X ^ POS.




guid_type(null).
guid_type(fccy463).
guid_type(srce544).
guid_type(ddd213).
guid_type(tchc397).
guid_type(mmm636).


map_guid(null,POS,VAL) :-
        positional_base(X),
        VAL is 0 * X ^ POS.
map_guid(fccy463,POS,VAL) :-
        positional_base(X),
        VAL is 1 * X ^ POS.
map_guid(srce544,POS,VAL) :-
        positional_base(X),
        VAL is 2 * X ^ POS.
map_guid(tchc397,POS,VAL) :-
        positional_base(X),
        VAL is 3 * X ^ POS.
map_guid(mmm636,POS,VAL) :-
        positional_base(X),
        VAL is 4 * X ^ POS.
map_guid(ddd213,POS,VAL) :-
        positional_base(X),
        VAL is 5 * X ^ POS.



product_string_type(null).
product_string_type(aspirin).
product_string_type(ibuprofen).
%product_string_type(guaifenesin).
product_string_type(pseudoephedrine).
product_string_type(chlorpheniramine).
product_string_type(hydrocodone).

map_product(null,POS,VAL) :-
        positional_base(X),
        VAL is 0 * X ^ POS.
map_product(pseudoephedrine,POS,VAL) :-
        positional_base(X),
        VAL is 1 * X ^ POS.
map_product(chlorpheniramine,POS,VAL) :-
        positional_base(X),
        VAL is 2 * X ^ POS.
map_product(hydrocodone,POS,VAL) :-
        positional_base(X),
        VAL is 3 * X ^ POS.
map_product(ibuprofen,POS,VAL) :-
        positional_base(X),
        VAL is 4 * X ^ POS.
map_product(aspirin,POS,VAL) :-
        positional_base(X),
        VAL is 5 * X ^ POS.


title_string_type(null).
title_string_type(ms).
title_string_type(mr).
title_string_type(dr).
title_string_type(mrs).
title_string_type(esq).


map_title(null,POS,VAL) :-
        positional_base(X),
        VAL is 0 * X ^ POS.
map_title(ms,POS,VAL) :-
        positional_base(X),
        VAL is 1 * X ^ POS.
map_title(mr,POS,VAL) :-
        positional_base(X),
        VAL is 2 * X ^ POS.
map_title(dr,POS,VAL) :-
        positional_base(X),
        VAL is 3 * X ^ POS.
map_title(esq,POS,VAL) :-
        positional_base(X),
        VAL is 4 * X ^ POS.
map_title(mrs,POS,VAL) :-
        positional_base(X),
        VAL is 5 * X ^ POS.

salary_type(null).
salary_type(4500).
salary_type(850).
salary_type(3100).
salary_type(990).
salary_type(935).


map_salary(null,POS,VAL) :-
        positional_base(X),
        VAL is 0 * X ^ POS.
map_salary(4500,POS,VAL) :-
        positional_base(X),
        VAL is 1 * X ^ POS.
map_salary(850,POS,VAL) :-
        positional_base(X),
        VAL is 2 * X ^ POS.
map_salary(3100,POS,VAL) :-
        positional_base(X),
        VAL is 3 * X ^ POS.
map_salary(990,POS,VAL) :-
        positional_base(X),
        VAL is 4 * X ^ POS.
map_salary(935,POS,VAL) :-
        positional_base(X),
        VAL is 5 * X ^ POS.
