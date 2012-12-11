:- module(datatypes,
          [isnull/1,            % only one symbol is null, and that is: 'null'
           not_null/1,
           xxx_hide_nonnull/1,
           int_type/1,          % -2, -1, 0, 1 ... (and null)
           natural_type/1,      % 0, 1, 2 ... (and null)
           name_string_type/1,  % jacob, isabella ... (and null)
           word_type/1,         % dance, rural ... (and null)
           guid_type/1,         % fccy463, srce544 ... (and null)
           product_string_type/1, % aspirin, ibuprofen ... (and null)

           end_of_d1_exports_placeholder/0]).   % this is here so i don't have to move the ']).' each time i add to exports

end_of_d1_exports_placeholder.

isnull(null).

% remember: we are talking about X should be INSTANTIATED ALREADY
not_null(X) :- \+isnull(X).


% note the use of so-called 'green cut' here.
% (ok... so not 100% green to overlap between int_type and natural_type, but this actually is still ok).
% once we SUCCEED AT PROVING that something is nonnull, we no longer care to investigate the other nonnull paths
xxx_hide_nonnull(X) :- int_type(X),  !, \+isnull(X).
xxx_hide_nonnull(X) :- name_string_type(X),    !, \+isnull(X).
xxx_hide_nonnull(X) :- word_type(X),  !, \+isnull(X).
xxx_hide_nonnull(X) :- guid_type(X), !, \+isnull(X).
xxx_hide_nonnull(X) :- natural_type(X),  !, \+isnull(X).

% without the following, other atoms not 'typed' in this file will FAIL a 'nonnull' test.
% for example:  nonnull(abcdefghijk).  will come out FALSE.
xxx_hide_nonnull(X) :- \+int_type(X), \+name_string_type(X), \+word_type(X), \+guid_type(X), \+natural_type(X),
              \+isnull(X).


natural_type(null).
natural_type(0).
natural_type(1).
natural_type(2).
% natural_type(3).
% natural_type(4).

int_type(X) :- natural_type(X).

int_type(-2).
int_type(-1).


name_string_type(null).
name_string_type(jacob).
name_string_type(isabella).
name_string_type(william).
%name_string_type(olivia).
% name_string_type(noah).
% name_string_type(emily).



word_type(null).
word_type(dance).
word_type(rural).
word_type(noise).



guid_type(null).
guid_type(fccy463).
guid_type(srce544).
guid_type(ddd213).
% guid_type(tchc397).
% guid_type(mmm636).
% guid_type(eght400).
% guid_type(avhi158).
% guid_type(shlm647).


product_string_type(null).
product_string_type(aspirin).
product_string_type(ibuprofen).
%product_string_type(guaifenesin).