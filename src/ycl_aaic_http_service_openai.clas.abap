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

    TYPES: BEGIN OF ty_message_s,
             id       TYPE string,
             seqno    TYPE yaaic_msg-seqno,
             msg      TYPE /ui2/cl_json=>json,
             msg_date TYPE yaaic_msg-msg_date,
             msg_time TYPE yaaic_msg-msg_time,
           END OF ty_message_s,

           ty_message_t TYPE STANDARD TABLE OF ty_message_s WITH EMPTY KEY,

           BEGIN OF ty_request,
             chatid  TYPE string,
             apikey  TYPE string,
             prompt  TYPE string,
             context TYPE string,
             model   TYPE string,
           END OF ty_request,

           BEGIN OF ty_response,
             chatid   TYPE string,
             message  TYPE string,
             messages TYPE ty_message_t,
           END OF ty_response.

    DATA: ls_request  TYPE ty_request,
          ls_response TYPE ty_response.

    DATA l_response_text TYPE string.

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

            DATA(l_model) = request->get_form_field( i_name = 'model' ).

            DATA(l_agent_id) = request->get_form_field( i_name = 'agent' ).

            IF l_model IS NOT INITIAL AND ls_request-model IS INITIAL.
              ls_request-model = l_model.
            ENDIF.

            DATA(lo_aaic_conn) = NEW ycl_aaic_conn( i_api = yif_aaic_const=>c_openai ).

            lo_aaic_conn->set_api_key( i_api_key = ls_request-apikey ).

            DATA(lo_aaic_db) = NEW ycl_aaic_db( i_api = yif_aaic_const=>c_openai
                                                i_id = CONV #( ls_request-chatid ) ).


            DATA(lo_aaic_openai) = NEW ycl_aaic_openai( i_model = ls_request-model
                                                        i_o_connection = lo_aaic_conn
                                                        i_o_persistence = lo_aaic_db ).

            IF l_agent_id IS NOT INITIAL.

              DATA(lo_agent) = NEW ycl_aaic_agent(
                i_agent_id = CONV #( l_agent_id )
                i_chat_id  = lo_aaic_db->m_id
              ).

              DATA(l_system_instructions) = lo_agent->get_system_instructions( ).

              lo_aaic_openai->set_system_instructions(
                i_system_instructions = l_system_instructions
              ).

            ENDIF.

            IF ls_request-context IS INITIAL.

              lo_aaic_openai->chat(
                EXPORTING
                  i_message  = ls_request-prompt
                  i_o_agent  = lo_agent
                IMPORTING
                  e_response = ls_response-message
              ).

            ELSE.

              " Default template
              DATA(l_prompt_template) = |**User message**: %USER_MESSAGE% \n\n**Context**:\n\n %CONTEXT% \n\n|.

              IF l_agent_id IS NOT INITIAL.

                l_prompt_template = lo_agent->get_prompt_template( CONV #( l_agent_id ) ).

              ENDIF.

              DATA(lo_aaic_prompt_template) = NEW ycl_aaic_prompt_template(
                i_template_text = l_prompt_template
              ).

              DATA(lo_aaic_prompt) = NEW ycl_aaic_prompt(
                i_o_prompt_template = lo_aaic_prompt_template
                i_s_params = VALUE yif_aaic_prompt=>ty_params_basic_s( user_message = ls_request-prompt
                                                                       context = ls_request-context )
              ).

              lo_aaic_openai->chat(
                EXPORTING
                  i_o_prompt = lo_aaic_prompt
                  i_o_agent  = lo_agent
                IMPORTING
                  e_response = ls_response-message
              ).

            ENDIF.

            ls_response-chatid = lo_aaic_db->m_id.

            lo_aaic_db->get_chat(
              EXPORTING
                i_ui = abap_true
              IMPORTING
                e_t_messages = DATA(lt_messages)
            ).

            ls_response-messages = CORRESPONDING #( lt_messages ).

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

            DATA(l_format) = request->get_form_field( i_name = 'format' ).

            DATA(lo_aaic_db_get) = NEW ycl_aaic_db( i_api = yif_aaic_const=>c_openai
                                                    i_id = CONV #( l_chat_id ) ).

            IF l_format IS INITIAL.

              DATA(l_html) = NEW ycl_aaic_ui_chat( )->get_html( i_api = yif_aaic_const=>c_openai
                                                                i_chat_id = CONV #( lo_aaic_db_get->m_id ) ).

              response->set_text(
                EXPORTING
                  i_text = l_html
              ).

              RETURN.

            ENDIF.

            lo_aaic_db_get->get_chat(
              EXPORTING
                i_ui = abap_true
              IMPORTING
                e_t_messages = lt_messages
            ).

            l_format = to_lower( l_format ).

            CASE l_format.

              WHEN 'json'.

                l_response_text = '[]'.

                IF lt_messages[] IS NOT INITIAL.

                  DATA(lt_messages_aux) = CORRESPONDING ty_message_t( lt_messages ).

                  l_response_text = NEW ycl_aaic_util( )->serialize(
                    EXPORTING
                      i_data = lt_messages_aux
                  ).

                ENDIF.

                l_response_text = '{"messages":' && l_response_text && '}'.

                response->set_content_type( content_type = 'application/json' ).

                response->set_text(
                  EXPORTING
                   i_text = l_response_text
                ).

              WHEN 'xml'.

                CALL TRANSFORMATION id
                  SOURCE messages = lt_messages
                  RESULT XML l_response_text.

                response->set_text(
                  EXPORTING
                   i_text = l_response_text
                ).

              WHEN 'mermaid'.

                response->set_text(
                  EXPORTING
                   i_text = NEW ycl_aaic_diagram_openai( )->get_diagram( l_chat_id )
                ).

              WHEN OTHERS.

                response->set_text(
                  EXPORTING
                   i_text = |The requested format "{ l_format }" is not supported|
                ).

            ENDCASE.

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
