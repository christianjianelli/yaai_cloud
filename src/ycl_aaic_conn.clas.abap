CLASS ycl_aaic_conn DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES yif_aaic_conn.

    ALIASES on_request_send FOR yif_aaic_conn~on_request_send.
    ALIASES on_response_received FOR yif_aaic_conn~on_response_received.
    ALIASES on_connection_error FOR yif_aaic_conn~on_connection_error.

    ALIASES mo_log FOR yif_aaic_conn~mo_log.
    ALIASES mo_api_key FOR yif_aaic_conn~mo_api_key.
    ALIASES mt_msg FOR yif_aaic_conn~mt_msg.
    ALIASES m_api FOR yif_aaic_conn~m_api.
    ALIASES m_base_url FOR yif_aaic_conn~m_base_url.
    ALIASES m_username FOR yif_aaic_conn~m_username.
    ALIASES m_password FOR yif_aaic_conn~m_password.
    ALIASES m_get_x_csrf_token FOR yif_aaic_conn~m_get_x_csrf_token.

    ALIASES create FOR yif_aaic_conn~create.
    ALIASES set_body FOR yif_aaic_conn~set_body.
    ALIASES execute FOR yif_aaic_conn~execute.
    ALIASES get_last_response FOR yif_aaic_conn~get_last_response.
    ALIASES set_api_key FOR yif_aaic_conn~set_api_key.
    ALIASES set_base_url FOR yif_aaic_conn~set_base_url.
    ALIASES get_error_text FOR yif_aaic_conn~get_error_text.

    METHODS
      constructor
        IMPORTING
          i_api              TYPE string OPTIONAL
          i_username         TYPE csequence OPTIONAL
          i_password         TYPE csequence OPTIONAL
          i_get_x_csrf_token TYPE abap_bool DEFAULT abap_false
          i_o_api_key        TYPE REF TO yif_aaic_api_key OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _o_http_client   TYPE REF TO if_web_http_client,
          _o_http_response TYPE REF TO if_web_http_response.

    DATA: _url      TYPE string,
          _api_key  TYPE string,
          _response TYPE string.

    METHODS
      log
        IMPORTING
          i_s_msg TYPE bapiret2.

ENDCLASS.



CLASS ycl_aaic_conn IMPLEMENTATION.

  METHOD constructor.

    IF i_api IS SUPPLIED.

      SELECT SINGLE base_url
        FROM yaaic_base_url
        WHERE id = @i_api
        INTO @me->m_base_url.

    ENDIF.

    IF i_o_api_key IS SUPPLIED.
      me->mo_api_key = i_o_api_key.
    ENDIF.

    IF i_username IS SUPPLIED.
      me->m_username = i_username.
    ENDIF.

    IF i_password IS SUPPLIED.
      me->m_password = i_password.
    ENDIF.

    IF i_get_x_csrf_token IS SUPPLIED.
      me->m_get_x_csrf_token = i_get_x_csrf_token.
    ENDIF.

  ENDMETHOD.


  METHOD log.

    IF me->mo_log IS NOT BOUND.
      me->mo_log = NEW #( ).
    ENDIF.

    me->mo_log->add( i_s_msg = i_s_msg ).

    IF sy-msgid IS NOT INITIAL AND
       sy-msgty IS NOT INITIAL AND
       sy-msgno IS NOT INITIAL.

      me->mo_log->add( VALUE #( id = sy-msgid
                                type = sy-msgty
                                number = sy-msgno
                                message_v1 = sy-msgv1
                                message_v2 = sy-msgv2
                                message_v3 = sy-msgv3
                                message_v4 = sy-msgv4 ) ).
    ENDIF.

  ENDMETHOD.


  METHOD yif_aaic_conn~add_http_header_param.

    APPEND VALUE #( name = i_name
                    value = i_value ) TO me->yif_aaic_conn~mt_http_header.

  ENDMETHOD.


  METHOD yif_aaic_conn~create.

    r_created = abap_false.

    me->_url = me->m_base_url.

    IF i_api_key IS SUPPLIED.
      me->_api_key = i_api_key.
    ENDIF.

    DATA(l_len) = strlen( me->_url ).

    IF me->_url(l_len) = '/'.
      me->_url = shift_right( val = me->_url places = 1 ).
    ENDIF.

    IF i_endpoint IS NOT INITIAL AND i_endpoint(1) <> '/'.
      me->_url = |{ me->_url }/{ i_endpoint }|.
    ENDIF.

    IF i_endpoint IS NOT INITIAL AND i_endpoint(1) = '/'.
      me->_url = |{ me->_url }{ i_endpoint }|.
    ENDIF.

    IF me->_api_key IS INITIAL AND me->m_api IS NOT INITIAL.

      IF me->mo_api_key IS BOUND.

        me->set_api_key( i_api_key = me->mo_api_key->get( me->m_api ) ).

      ENDIF.

    ENDIF.

    TRY.

        DATA(lo_destination) = cl_http_destination_provider=>create_by_url( me->_url ).

      CATCH cx_http_dest_provider_error INTO DATA(lo_http_dest_provider_error).

        me->log( i_s_msg = VALUE #( number = '001' ) ).

        me->log( i_s_msg = VALUE #( id = lo_http_dest_provider_error->if_t100_message~t100key-msgid
                                    number = lo_http_dest_provider_error->if_t100_message~t100key-msgno
                                    message_v1 = CONV #( lo_http_dest_provider_error->if_t100_message~t100key-attr1 )
                                    message_v2 = CONV #( lo_http_dest_provider_error->if_t100_message~t100key-attr2 )
                                    message_v3 = CONV #( lo_http_dest_provider_error->if_t100_message~t100key-attr3 )
                                    message_v4 = CONV #( lo_http_dest_provider_error->if_t100_message~t100key-attr4 ) ) ).

        RAISE EVENT on_connection_error.

        RETURN.

    ENDTRY.

    TRY.

        me->_o_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).

      CATCH cx_web_http_client_error INTO DATA(lo_web_http_client_error).

        me->log( i_s_msg = VALUE #( number = '001' ) ).

        me->log( i_s_msg = VALUE #( id = lo_web_http_client_error->if_t100_message~t100key-msgid
                                    number = lo_web_http_client_error->if_t100_message~t100key-msgno
                                    message_v1 = CONV #( lo_web_http_client_error->if_t100_message~t100key-attr1 )
                                    message_v2 = CONV #( lo_web_http_client_error->if_t100_message~t100key-attr2 )
                                    message_v3 = CONV #( lo_web_http_client_error->if_t100_message~t100key-attr3 )
                                    message_v4 = CONV #( lo_web_http_client_error->if_t100_message~t100key-attr4 ) ) ).

        RAISE EVENT on_connection_error.

        RETURN.

    ENDTRY.

    DATA(lo_request) = me->_o_http_client->get_http_request( ).

    lo_request->set_content_type( content_type = 'application/json' ).

    r_created = abap_true.

  ENDMETHOD.


  METHOD yif_aaic_conn~execute.

    e_failed = abap_false.

    TRY.

        DATA(lo_request) = me->_o_http_client->get_http_request( ).

        "If the API expects to receive the API Key in the URL
        DATA(l_apikey_url_placeholder) = |{ yif_aaic_const=>c_placeholder_pattern }APIKEY{ yif_aaic_const=>c_placeholder_pattern }|.

        LOOP AT me->yif_aaic_conn~mt_http_header INTO DATA(ls_http_header).

          FIND l_apikey_url_placeholder IN ls_http_header-value.

          IF sy-subrc = 0.

            REPLACE l_apikey_url_placeholder IN ls_http_header-value WITH me->_api_key.

            DATA(l_skip_bearer_http_header) = abap_true.

          ENDIF.

          TRY.

              lo_request->set_header_field(
                EXPORTING
                  i_name  = ls_http_header-name
                  i_value = ls_http_header-value
              ).

            CATCH cx_web_message_error ##NO_HANDLER.
              CONTINUE.
          ENDTRY.

        ENDLOOP.

        IF me->_api_key IS NOT INITIAL AND l_skip_bearer_http_header = abap_false.

          lo_request->set_authorization_bearer(
            EXPORTING
            i_bearer = me->_api_key
          ).

        ENDIF.

        IF me->m_username IS NOT INITIAL AND me->m_password IS NOT INITIAL.

          lo_request->set_authorization_basic(
            EXPORTING
              i_username = me->m_username
              i_password = me->m_password
          ).

        ENDIF.

        IF me->m_get_x_csrf_token = abap_true.

          me->_o_http_client->set_csrf_token( ).

        ENDIF.

        me->_o_http_response = me->_o_http_client->execute( i_method = if_web_http_client=>post ).

        e_response = me->_o_http_response->get_text( ).

      CATCH cx_web_http_client_error INTO DATA(lo_web_http_client_error).

        me->log( i_s_msg = VALUE #( number = '002' ) ).

        me->log( i_s_msg = VALUE #( id = lo_web_http_client_error->if_t100_message~t100key-msgid
                                    number = lo_web_http_client_error->if_t100_message~t100key-msgno
                                    message_v1 = CONV #( lo_web_http_client_error->if_t100_message~t100key-attr1 )
                                    message_v2 = CONV #( lo_web_http_client_error->if_t100_message~t100key-attr2 )
                                    message_v3 = CONV #( lo_web_http_client_error->if_t100_message~t100key-attr3 )
                                    message_v4 = CONV #( lo_web_http_client_error->if_t100_message~t100key-attr4 ) ) ).

        e_failed = abap_true.

        RAISE EVENT on_connection_error.

        RETURN.

      CATCH cx_web_message_error INTO DATA(lo_web_message_error).

        me->log( i_s_msg = VALUE #( number = '002' ) ).

        me->log( i_s_msg = VALUE #( id = lo_web_message_error->if_t100_message~t100key-msgid
                                    number = lo_web_message_error->if_t100_message~t100key-msgno
                                    message_v1 = CONV #( lo_web_message_error->if_t100_message~t100key-attr1 )
                                    message_v2 = CONV #( lo_web_message_error->if_t100_message~t100key-attr2 )
                                    message_v3 = CONV #( lo_web_message_error->if_t100_message~t100key-attr3 )
                                    message_v4 = CONV #( lo_web_message_error->if_t100_message~t100key-attr4 ) ) ).

        e_failed = abap_true.

        RAISE EVENT on_connection_error.

        RETURN.

    ENDTRY.

  ENDMETHOD.


  METHOD yif_aaic_conn~get_error_text.

    DATA l_text TYPE string.

    CLEAR e_error_text.

    LOOP AT me->mo_log->mt_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).

      IF <ls_msg>-id IS INITIAL OR <ls_msg>-type <> 'E' OR <ls_msg>-number IS INITIAL.
        CONTINUE.
      ENDIF.

      MESSAGE ID <ls_msg>-id
        TYPE <ls_msg>-type
        NUMBER <ls_msg>-number
        WITH <ls_msg>-message_v1
             <ls_msg>-message_v2
             <ls_msg>-message_v3
             <ls_msg>-message_v4
        INTO l_text.

      e_error_text = |{ e_error_text } ; { l_text }|.

    ENDLOOP.

  ENDMETHOD.


  METHOD yif_aaic_conn~get_last_response.

    IF me->_o_http_response IS BOUND.

      r_response = me->_o_http_response.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aaic_conn~remove_http_header_param.

    DELETE me->yif_aaic_conn~mt_http_header WHERE name = i_name.

  ENDMETHOD.


  METHOD yif_aaic_conn~set_api_key.

    me->_api_key = i_api_key.

    IF i_o_api_key IS SUPPLIED.

      me->mo_api_key = i_o_api_key.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aaic_conn~set_base_url.

    me->m_base_url = i_base_url.

  ENDMETHOD.


  METHOD yif_aaic_conn~set_body.

    DATA(lo_request) = me->_o_http_client->get_http_request( ).

    lo_request->set_text( i_json ).

  ENDMETHOD.

ENDCLASS.
