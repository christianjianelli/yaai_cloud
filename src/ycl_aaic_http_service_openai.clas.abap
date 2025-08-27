CLASS ycl_aaic_http_service_openai DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aaic_http_service_openai IMPLEMENTATION.

  METHOD if_http_service_extension~handle_request.

    TYPES: BEGIN OF ty_request,
             chatid TYPE string,
             apikey TYPE string,
             prompt TYPE string,
             model  TYPE string,
           END OF ty_request,

           BEGIN OF ty_response,
             message TYPE string,
             chatid  TYPE string,
           END OF ty_response.

    DATA: ls_request  TYPE ty_request,
          ls_response TYPE ty_response.

    TRY.

        DATA(l_method) = request->get_method( ).

        CASE l_method.

          WHEN 'POST'.

            DATA(l_json) = request->get_text( ).

            /ui2/cl_json=>deserialize(
              EXPORTING
                json = l_json
              CHANGING
                data = ls_request
            ).

            DATA(lo_aaic_conn) = NEW ycl_aaic_conn( i_api = yif_aaic_const=>c_openai ).

            lo_aaic_conn->set_api_key( i_api_key = ls_request-apikey ).

            DATA(lo_aaic_db) = NEW ycl_aaic_db( i_api = yif_aaic_const=>c_openai
                                                i_id = CONV #( ls_request-chatid ) ).

            DATA(lo_aaic_openai) = NEW ycl_aaic_openai( i_model = COND #( WHEN ls_request-model IS NOT INITIAL THEN ls_request-model ELSE 'gpt-4.1' )
                                                        i_o_connection = lo_aaic_conn
                                                        i_o_persistence = lo_aaic_db ).

            lo_aaic_openai->chat(
              EXPORTING
                i_message  = ls_request-prompt
              IMPORTING
                e_response = ls_response-message
            ).

            ls_response-chatid = lo_aaic_db->m_id.

            l_json = /ui2/cl_json=>serialize(
              EXPORTING
                data = ls_response
                pretty_name = /ui2/cl_json=>pretty_mode-low_case
            ).

            response->set_text(
              EXPORTING
                i_text = l_json
            ).

          WHEN 'GET'.

            DATA(l_chat_id) = request->get_form_field( i_name = 'chat_id' ).

            DATA(lo_aaic_db_get) = NEW ycl_aaic_db( i_api = yif_aaic_const=>c_openai
                                                    i_id = CONV #( l_chat_id ) ).

            DATA(l_html) = NEW ycl_aaic_ui_chat( )->get_html( i_api = yif_aaic_const=>c_openai
                                                              i_chat_id = CONV #( lo_aaic_db_get->m_id ) ).

            response->set_text(
              EXPORTING
                i_text = l_html
            ).

          WHEN OTHERS.

            response->set_status(
              EXPORTING
                i_code   = '405'
                i_reason = 'Method not allowed'
            ).

            RETURN.

        ENDCASE.

      CATCH cx_web_message_error.

        response->set_text(
          EXPORTING
            i_text = 'Error ... 😕'
        ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
