-module(iso_8583_fields_2003_tests).

-include_lib("eunit/include/eunit.hrl").

-include("iso_8583_fields_id.hrl").

%%====================================================================
%% Test Core Fields (0-1)
%%====================================================================

get_encoding_mti_test() ->
    ?assertEqual({n, fixed, 4}, iso_8583_fields_2003:get_encoding(?MTI)).

get_encoding_bitmap_extended_test() ->
    ?assertEqual({b, fixed, 64}, iso_8583_fields_2003:get_encoding(?BITMAP_EXTENDED)).

%%====================================================================
%% Test Fields Changed/Enhanced in ISO 8583:2003
%%====================================================================

%% Field 12 - Date and Time, Local Transaction - Extended from 12 to 14 digits
get_encoding_field_12_test() ->
    % In 2003: 14 digits (YYMMDDhhmmss with extended precision)
    ?assertEqual({n, fixed, 14},
                 iso_8583_fields_2003:get_encoding(?DATE_AND_TIME_LOCAL_TRAN)),
    % In 1993: 12 digits (YYMMDDHHMMSS)
    ?assertEqual({n, fixed, 12},
                 iso_8583_fields_1993:get_encoding(?DATE_AND_TIME_LOCAL_TRAN)),
    % Verify they're different
    ?assertNotEqual(iso_8583_fields_2003:get_encoding(?DATE_AND_TIME_LOCAL_TRAN),
                    iso_8583_fields_1993:get_encoding(?DATE_AND_TIME_LOCAL_TRAN)).

%% Field 22 - POS Data Code
get_encoding_field_22_test() ->
    % Same as 1993
    ?assertEqual({an, fixed, 12}, iso_8583_fields_2003:get_encoding(?POS_DATA_CODE)),
    ?assertEqual({an, fixed, 12}, iso_8583_fields_1993:get_encoding(?POS_DATA_CODE)),
    ?assertEqual(iso_8583_fields_2003:get_encoding(?POS_DATA_CODE),
                 iso_8583_fields_1993:get_encoding(?POS_DATA_CODE)).

%% Field 43 - Card Acceptor Name/Location - Changed format
get_encoding_field_43_test() ->
    % In 2003: ans fixed 99
    ?assertEqual({ans, fixed, 99},
                 iso_8583_fields_2003:get_encoding(?CARD_ACCEPTOR_NAME_LOCATION)),
    % In 1993: an llvar 99
    ?assertEqual({an, llvar, 99},
                 iso_8583_fields_1993:get_encoding(?CARD_ACCEPTOR_NAME_LOCATION)),
    % Verify type changed from an to ans
    ?assertNotEqual(element(1,
                            iso_8583_fields_2003:get_encoding(?CARD_ACCEPTOR_NAME_LOCATION)),
                    element(1, iso_8583_fields_1993:get_encoding(?CARD_ACCEPTOR_NAME_LOCATION))),
    % Verify length type changed from llvar to fixed
    ?assertNotEqual(element(2,
                            iso_8583_fields_2003:get_encoding(?CARD_ACCEPTOR_NAME_LOCATION)),
                    element(2, iso_8583_fields_1993:get_encoding(?CARD_ACCEPTOR_NAME_LOCATION))).

%% Field 48 - Additional Data - Private
get_encoding_field_48_test() ->
    ?assertEqual({ans, lllvar, 999},
                 iso_8583_fields_2003:get_encoding(?ADDITIONAL_DATA_PRIVATE)).

%% Field 53 - Security Related Control Information - Extended
get_encoding_field_53_test() ->
    % In 2003: an llvar 48
    ?assertEqual({an, llvar, 48},
                 iso_8583_fields_2003:get_encoding(?SECURITY_RELATED_CONTROL_INFO)),
    % In 1993: an llvar 8
    ?assertEqual({an, llvar, 8},
                 iso_8583_fields_1993:get_encoding(?SECURITY_RELATED_CONTROL_INFO)),
    % Verify max length changed
    ?assertNotEqual(element(3,
                            iso_8583_fields_2003:get_encoding(?SECURITY_RELATED_CONTROL_INFO)),
                    element(3, iso_8583_fields_1993:get_encoding(?SECURITY_RELATED_CONTROL_INFO))).

%% Field 54 - Additional Amounts - Extended
get_encoding_field_54_test() ->
    % In 2003: an lllvar 255
    ?assertEqual({an, lllvar, 255}, iso_8583_fields_2003:get_encoding(?ADDITIONAL_AMOUNTS)),
    % In 1993: an lllvar 120
    ?assertEqual({an, lllvar, 120}, iso_8583_fields_1993:get_encoding(?ADDITIONAL_AMOUNTS)),
    % Verify max length increased
    ?assert(element(3, iso_8583_fields_2003:get_encoding(?ADDITIONAL_AMOUNTS))
            > element(3, iso_8583_fields_1993:get_encoding(?ADDITIONAL_AMOUNTS))).

%% Field 55 - ICC System Related Data (EMV)
get_encoding_field_55_test() ->
    ?assertEqual({ans, lllvar, 999},
                 iso_8583_fields_2003:get_encoding(?ICC_SYSTEM_RELATED_DATA)),
    % Same as 1993
    ?assertEqual(iso_8583_fields_2003:get_encoding(?ICC_SYSTEM_RELATED_DATA),
                 iso_8583_fields_1993:get_encoding(?ICC_SYSTEM_RELATED_DATA)).

%% Field 62 - Private Use (Reserved Private 2)
get_encoding_field_62_test() ->
    ?assertEqual({ans, lllvar, 999}, iso_8583_fields_2003:get_encoding(?RESERVED_PRIVATE2)).

%% Field 63 - Private Use (Reserved Private 3)
get_encoding_field_63_test() ->
    ?assertEqual({ans, lllvar, 999}, iso_8583_fields_2003:get_encoding(?RESERVED_PRIVATE3)).

%% Field 65 - Tertiary Bitmap
get_encoding_field_65_test() ->
    ?assertEqual({b, fixed, 64}, iso_8583_fields_2003:get_encoding(?BITMAP_TERTIARY)),
    % Same as 1993
    ?assertEqual(iso_8583_fields_2003:get_encoding(?BITMAP_TERTIARY),
                 iso_8583_fields_1993:get_encoding(?BITMAP_TERTIARY)).

%% Field 96 - Message Security Code - Enhanced
get_encoding_field_96_test() ->
    % In 2003: an lllvar 32
    ?assertEqual({an, lllvar, 32}, iso_8583_fields_2003:get_encoding(?MESSAGE_SECURITY_CODE)),
    % In 1993: an llvar 18
    ?assertEqual({an, llvar, 18}, iso_8583_fields_1993:get_encoding(?MESSAGE_SECURITY_CODE)),
    % Verify length type changed from llvar to lllvar
    ?assertNotEqual(element(2, iso_8583_fields_2003:get_encoding(?MESSAGE_SECURITY_CODE)),
                    element(2, iso_8583_fields_1993:get_encoding(?MESSAGE_SECURITY_CODE))),
    % Verify max length increased
    ?assert(element(3, iso_8583_fields_2003:get_encoding(?MESSAGE_SECURITY_CODE))
            > element(3, iso_8583_fields_1993:get_encoding(?MESSAGE_SECURITY_CODE))).

%% Field 102 - Account Identification 1
get_encoding_field_102_test() ->
    ?assertEqual({ans, llvar, 28}, iso_8583_fields_2003:get_encoding(?ACCOUNT_ID1)).

%% Field 103 - Account Identification 2
get_encoding_field_103_test() ->
    ?assertEqual({ans, llvar, 28}, iso_8583_fields_2003:get_encoding(?ACCOUNT_ID2)).

%%====================================================================
%% Test Extended Fields (129-192)
%%====================================================================

get_encoding_extended_fields_test_() ->
    [?_assertEqual({ans, lllvar, 999}, iso_8583_fields_2003:get_encoding(129)),
     ?_assertEqual({ans, lllvar, 999}, iso_8583_fields_2003:get_encoding(130)),
     ?_assertEqual({ans, lllvar, 999}, iso_8583_fields_2003:get_encoding(150)),
     ?_assertEqual({ans, lllvar, 999}, iso_8583_fields_2003:get_encoding(175)),
     ?_assertEqual({ans, lllvar, 999}, iso_8583_fields_2003:get_encoding(192))].

extended_fields_all_have_same_encoding_test() ->
    ExtendedFields = lists:seq(129, 192),
    Encodings = [iso_8583_fields_2003:get_encoding(Id) || Id <- ExtendedFields],
    UniqueEncodings = lists:usort(Encodings),
    ?assertEqual(1, length(UniqueEncodings)),
    ?assertEqual({ans, lllvar, 999}, hd(UniqueEncodings)).

%%====================================================================
%% Test List-Based Field IDs
%%====================================================================

get_encoding_list_based_test_() ->
    [?_assertEqual({n, fixed, 4}, iso_8583_fields_2003:get_encoding([?MTI])),
     ?_assertEqual({n, fixed, 14},
                   iso_8583_fields_2003:get_encoding([?DATE_AND_TIME_LOCAL_TRAN])),
     ?_assertEqual({ans, lllvar, 999}, iso_8583_fields_2003:get_encoding([129]))].

%%====================================================================
%% Test Delegation to 1993
%%====================================================================

delegation_to_1993_test_() ->
    UnchangedFields =
        [?PAN,
         ?PROC_CODE,
         ?AMOUNT_TRAN,
         ?SYSTEMS_TRACE_AUDIT_NUMBER,
         ?TRANSMISSION_DATE_TIME,
         ?RETRIEVAL_REF_NUM,
         ?AUTHORIZATION_ID_RESP,
         ?RESP_CODE,
         ?CARD_ACCEPTOR_TERMINAL_ID,
         ?TRACK_2_DATA],
    [?_assertEqual(iso_8583_fields_1993:get_encoding(Field),
                   iso_8583_fields_2003:get_encoding(Field))
     || Field <- UnchangedFields].

%%====================================================================
%% Test Invalid Field IDs
%%====================================================================

get_encoding_invalid_field_test() ->
    ?assertThrow(invalid_field_id, iso_8583_fields_2003:get_encoding(193)),
    ?assertThrow(invalid_field_id, iso_8583_fields_2003:get_encoding(250)).

%%====================================================================
%% Test get_field_name/1 - Extended Fields
%%====================================================================

get_field_name_extended_test_() ->
    [?_assertEqual(<<"Date and Time, Local Transaction (Extended)">>,
                   iso_8583_fields_2003:get_field_name(?DATE_AND_TIME_LOCAL_TRAN)),
     ?_assertEqual(<<"Security Related Control Information (Extended)">>,
                   iso_8583_fields_2003:get_field_name(?SECURITY_RELATED_CONTROL_INFO)),
     ?_assertEqual(<<"Additional Amounts (Extended)">>,
                   iso_8583_fields_2003:get_field_name(?ADDITIONAL_AMOUNTS)),
     ?_assertEqual(<<"Message Security Code (Enhanced)">>,
                   iso_8583_fields_2003:get_field_name(?MESSAGE_SECURITY_CODE))].

%%====================================================================
%% Test get_field_name/1 - Extended Field Range (129-192)
%%====================================================================

get_field_name_extended_range_test_() ->
    [?_assertEqual(<<"Extended Field 129">>, iso_8583_fields_2003:get_field_name(129)),
     ?_assertEqual(<<"Extended Field 150">>, iso_8583_fields_2003:get_field_name(150)),
     ?_assertEqual(<<"Extended Field 192">>, iso_8583_fields_2003:get_field_name(192))].

%%====================================================================
%% Test get_field_name/1 - Delegation to 1993
%%====================================================================

get_field_name_delegation_test_() ->
    [?_assertEqual(iso_8583_fields_1993:get_field_name(?MTI),
                   iso_8583_fields_2003:get_field_name(?MTI)),
     ?_assertEqual(iso_8583_fields_1993:get_field_name(?PAN),
                   iso_8583_fields_2003:get_field_name(?PAN)),
     ?_assertEqual(iso_8583_fields_1993:get_field_name(?PROC_CODE),
                   iso_8583_fields_2003:get_field_name(?PROC_CODE))].

%%====================================================================
%% Test get_field_name/1 - Generic Fields
%%====================================================================

get_field_name_generic_test() ->
    % Fields beyond 192
    ?assertEqual(<<"Field 200">>, iso_8583_fields_2003:get_field_name(200)).

%%====================================================================
%% Test is_valid_field/1
%%====================================================================

is_valid_field_test_() ->
    [% Valid fields
     ?_assert(iso_8583_fields_2003:is_valid_field(0)),
     ?_assert(iso_8583_fields_2003:is_valid_field(1)),
     ?_assert(iso_8583_fields_2003:is_valid_field(64)),
     ?_assert(iso_8583_fields_2003:is_valid_field(128)),
     ?_assert(iso_8583_fields_2003:is_valid_field(129)),
     ?_assert(iso_8583_fields_2003:is_valid_field(150)),
     ?_assert(iso_8583_fields_2003:is_valid_field(192)),
     % Invalid fields
     ?_assertNot(iso_8583_fields_2003:is_valid_field(-1)),
     ?_assertNot(iso_8583_fields_2003:is_valid_field(193)),
     ?_assertNot(iso_8583_fields_2003:is_valid_field(200)),
     ?_assertNot(iso_8583_fields_2003:is_valid_field(255))].

is_valid_field_boundary_test() ->
    % Test boundary conditions
    ?assert(iso_8583_fields_2003:is_valid_field(0)),
    ?assert(iso_8583_fields_2003:is_valid_field(192)),
    ?assertNot(iso_8583_fields_2003:is_valid_field(193)).

%%====================================================================
%% Test get_all_fields/0
%%====================================================================

get_all_fields_test() ->
    AllFields = iso_8583_fields_2003:get_all_fields(),
    ?assertEqual(193, length(AllFields)),
    ?assertEqual(0, hd(AllFields)),
    ?assertEqual(192, lists:last(AllFields)).

get_all_fields_sequential_test() ->
    AllFields = iso_8583_fields_2003:get_all_fields(),
    ?assertEqual(lists:seq(0, 192), AllFields).

get_all_fields_includes_extended_test() ->
    AllFields = iso_8583_fields_2003:get_all_fields(),
    % Verify extended fields are included
    ?assert(lists:member(129, AllFields)),
    ?assert(lists:member(150, AllFields)),
    ?assert(lists:member(192, AllFields)).

%%====================================================================
%% Integration Tests
%%====================================================================

all_fields_have_encoding_test() ->
    AllFields = iso_8583_fields_2003:get_all_fields(),
    lists:foreach(fun(FieldId) ->
                     Encoding = iso_8583_fields_2003:get_encoding(FieldId),
                     ?assert(is_tuple(Encoding)),
                     ?assertEqual(3, tuple_size(Encoding))
                  end,
                  AllFields).

all_fields_are_valid_test() ->
    AllFields = iso_8583_fields_2003:get_all_fields(),
    lists:foreach(fun(FieldId) -> ?assert(iso_8583_fields_2003:is_valid_field(FieldId)) end,
                  AllFields).

all_fields_have_name_test() ->
    AllFields = iso_8583_fields_2003:get_all_fields(),
    lists:foreach(fun(FieldId) ->
                     Name = iso_8583_fields_2003:get_field_name(FieldId),
                     ?assert(is_binary(Name)),
                     ?assert(byte_size(Name) > 0)
                  end,
                  AllFields).

%%====================================================================
%% Test ISO 8583:2003 Specific Features
%%====================================================================

%% Extended field support (129-192)
extended_field_support_test() ->
    % Verify all extended fields are valid
    ExtendedFields = lists:seq(129, 192),
    lists:foreach(fun(FieldId) ->
                     ?assert(iso_8583_fields_2003:is_valid_field(FieldId)),
                     ?assertEqual({ans, lllvar, 999}, iso_8583_fields_2003:get_encoding(FieldId))
                  end,
                  ExtendedFields).

%% Enhanced security fields
enhanced_security_test_() ->
    [% Field 53 - Security Related Control Info extended to 48 chars
     ?_assertEqual({an, llvar, 48},
                   iso_8583_fields_2003:get_encoding(?SECURITY_RELATED_CONTROL_INFO)),
     % Field 96 - Message Security Code enhanced with lllvar
     ?_assertEqual({an, lllvar, 32},
                   iso_8583_fields_2003:get_encoding(?MESSAGE_SECURITY_CODE))].

%% Extended timestamp support
extended_timestamp_test() ->
    % Field 12 extended to 14 digits
    Encoding = iso_8583_fields_2003:get_encoding(?DATE_AND_TIME_LOCAL_TRAN),
    ?assertEqual({n, fixed, 14}, Encoding),
    ?assertEqual(14, element(3, Encoding)).

%% Tertiary bitmap support
tertiary_bitmap_support_test() ->
    % Field 65
    ?assertEqual({b, fixed, 64}, iso_8583_fields_2003:get_encoding(?BITMAP_TERTIARY)),
    ?assert(iso_8583_fields_2003:is_valid_field(?BITMAP_TERTIARY)).

%%====================================================================
%% Comparison Tests - 1993 vs 2003
%%====================================================================

field_count_comparison_test() ->
    Fields1993 = iso_8583_fields_1993:get_all_fields(),
    Fields2003 = iso_8583_fields_2003:get_all_fields(),
    ?assertEqual(129, length(Fields1993)),
    ?assertEqual(193, length(Fields2003)),
    ?assert(length(Fields2003) > length(Fields1993)).

fields_that_changed_test_() ->
    % List of field IDs that changed between 1993 and 2003
    ChangedFieldIds = [12, 43, 53, 54, 96],
    [?_assertNotEqual(iso_8583_fields_1993:get_encoding(FieldId),
                      iso_8583_fields_2003:get_encoding(FieldId))
     || FieldId <- ChangedFieldIds].

fields_enhanced_in_2003_test_() ->
    [% Field 12: 12 -> 14 digits
     ?_assert(element(3, iso_8583_fields_2003:get_encoding(12))
              > element(3, iso_8583_fields_1993:get_encoding(12))),
     % Field 53: 8 -> 48 chars
     ?_assert(element(3, iso_8583_fields_2003:get_encoding(53))
              > element(3, iso_8583_fields_1993:get_encoding(53))),
     % Field 54: 120 -> 255 chars
     ?_assert(element(3, iso_8583_fields_2003:get_encoding(54))
              > element(3, iso_8583_fields_1993:get_encoding(54))),
     % Field 96: 18 -> 32 chars, llvar -> lllvar
     ?_assert(element(3, iso_8583_fields_2003:get_encoding(96))
              > element(3, iso_8583_fields_1993:get_encoding(96)))].

%%====================================================================
%% Encoding Type Tests
%%====================================================================

encoding_types_used_test_() ->
    [% Numeric (n)
     ?_assertEqual(n, element(1, iso_8583_fields_2003:get_encoding(?MTI))),
     % Binary (b)
     ?_assertEqual(b, element(1, iso_8583_fields_2003:get_encoding(?BITMAP_EXTENDED))),
     ?_assertEqual(b, element(1, iso_8583_fields_2003:get_encoding(?BITMAP_TERTIARY))),
     % Alphanumeric (an)
     ?_assertEqual(an, element(1, iso_8583_fields_2003:get_encoding(?POS_DATA_CODE))),
     ?_assertEqual(an, element(1, iso_8583_fields_2003:get_encoding(?ADDITIONAL_AMOUNTS))),
     % Alphanumeric and special (ans)
     ?_assertEqual(ans,
                   element(1, iso_8583_fields_2003:get_encoding(?CARD_ACCEPTOR_NAME_LOCATION))),
     ?_assertEqual(ans,
                   element(1, iso_8583_fields_2003:get_encoding(?ICC_SYSTEM_RELATED_DATA))),
     ?_assertEqual(ans, element(1, iso_8583_fields_2003:get_encoding(129)))].

%%====================================================================
%% Length Type Tests
%%====================================================================

length_types_used_test_() ->
    [% Fixed length
     ?_assertEqual(fixed, element(2, iso_8583_fields_2003:get_encoding(?MTI))),
     ?_assertEqual(fixed, element(2, iso_8583_fields_2003:get_encoding(?BITMAP_EXTENDED))),
     % Variable length - llvar
     ?_assertEqual(llvar,
                   element(2, iso_8583_fields_2003:get_encoding(?SECURITY_RELATED_CONTROL_INFO))),
     ?_assertEqual(llvar, element(2, iso_8583_fields_2003:get_encoding(?ACCOUNT_ID1))),
     % Variable length - lllvar
     ?_assertEqual(lllvar, element(2, iso_8583_fields_2003:get_encoding(?ADDITIONAL_AMOUNTS))),
     ?_assertEqual(lllvar,
                   element(2, iso_8583_fields_2003:get_encoding(?MESSAGE_SECURITY_CODE))),
     ?_assertEqual(lllvar, element(2, iso_8583_fields_2003:get_encoding(129)))].

%%====================================================================
%% Edge Cases and Error Handling
%%====================================================================

boundary_fields_test_() ->
    [% First field
     ?_assert(iso_8583_fields_2003:is_valid_field(0)),
     % Last standard field
     ?_assert(iso_8583_fields_2003:is_valid_field(128)),
     % First extended field
     ?_assert(iso_8583_fields_2003:is_valid_field(129)),
     % Last extended field
     ?_assert(iso_8583_fields_2003:is_valid_field(192)),
     % Just beyond valid range
     ?_assertNot(iso_8583_fields_2003:is_valid_field(193))].

negative_field_id_test() ->
    % Negative field IDs are not valid
    ?assertNot(iso_8583_fields_2003:is_valid_field(-1)),
    ?assertNot(iso_8583_fields_2003:is_valid_field(-100)),
    % But get_encoding for negative IDs delegates to 1993, which may return default encoding
    % This is implementation behavior - not throwing exception
    Encoding = iso_8583_fields_2003:get_encoding(-1),
    ?assert(is_tuple(Encoding)),
    ?assertEqual(3, tuple_size(Encoding)).
