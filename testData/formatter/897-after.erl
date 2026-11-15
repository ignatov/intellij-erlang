pem_entry_decode({Asn1Type, CryptDer, {Cipher, Salt}} = PemEntry,
                 Password) when is_atom(Asn1Type) andalso
                                is_binary(CryptDer) andalso
                                is_list(Cipher) andalso
                                is_binary(Salt) andalso
                                ((erlang:byte_size(Salt) == 8) or (erlang:byte_size(Salt) == 16)) andalso
                                is_list(Password) ->
    do_pem_entry_decode(PemEntry, Password).

operators_first(A, B, C) when is_atom(A)
                              andalso is_binary(B)
                              andalso is_list(C)
                              andalso is_list(A) ->
    X = A
        orelse B
        orelse C,
    X.

comments(A, B, C) ->
    X = A andalso
        % first
        B andalso
        % second
        C andalso
        A andalso B,
    X.
