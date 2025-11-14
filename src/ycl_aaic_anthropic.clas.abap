CLASS ycl_aaic_anthropic DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aaic_anthropic.
    INTERFACES yif_aaic_chat.

    ALIASES on_message_send FOR yif_aaic_chat~on_message_send.
    ALIASES on_response_received FOR yif_aaic_chat~on_response_received.
    ALIASES on_message_failed FOR yif_aaic_chat~on_message_failed.

    ALIASES set_model FOR yif_aaic_anthropic~set_model.
    ALIASES set_temperature FOR yif_aaic_anthropic~set_temperature.
    ALIASES set_system_instructions FOR yif_aaic_anthropic~set_system_instructions.
    ALIASES set_connection FOR yif_aaic_anthropic~set_connection.
    ALIASES set_endpoint FOR yif_aaic_anthropic~set_endpoint.
    ALIASES bind_tools FOR yif_aaic_anthropic~bind_tools.
    ALIASES chat FOR yif_aaic_anthropic~chat.
    ALIASES get_conversation FOR yif_aaic_anthropic~get_conversation.

    ALIASES mo_function_calling FOR yif_aaic_anthropic~mo_function_calling.
    ALIASES m_anthropic_version FOR yif_aaic_anthropic~m_anthropic_version.
    ALIASES m_endpoint FOR yif_aaic_anthropic~m_endpoint.

    CLASS-DATA m_ref TYPE REF TO ycl_aaic_anthropic READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING
                i_model      TYPE csequence OPTIONAL
      RETURNING VALUE(r_ref) TYPE REF TO ycl_aaic_anthropic.

    METHODS constructor
      IMPORTING
        i_model         TYPE csequence OPTIONAL
        i_max_tokens    TYPE i OPTIONAL
        i_o_connection  TYPE REF TO yif_aaic_conn OPTIONAL
        i_o_persistence TYPE REF TO yif_aaic_db OPTIONAL.


  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _o_connection  TYPE REF TO yif_aaic_conn,
          _o_persistence TYPE REF TO yif_aaic_db.

    DATA: _model               TYPE string,
          _temperature         TYPE p LENGTH 2 DECIMALS 1,
          _max_tokens          TYPE i VALUE 2048,
          _system_instructions TYPE string,
          _chat_messages       TYPE yif_aaic_anthropic~ty_chat_messages_t,
          _max_tools_calls     TYPE i.

ENDCLASS.


CLASS ycl_aaic_anthropic IMPLEMENTATION.

  METHOD constructor.

    me->m_anthropic_version = '2023-06-01'.

    IF i_model IS NOT INITIAL.
      me->_model = i_model.
    ELSE.
      SELECT model FROM yaaic_model
        WHERE id = @yif_aaic_const=>c_anthropic
          AND default_model = @abap_true
        INTO @me->_model
        UP TO 1 ROWS.                                   "#EC CI_NOORDER
      ENDSELECT.
      IF sy-subrc <> 0.
        me->_model = 'claude-3-7-sonnet-latest'.
      ENDIF.
    ENDIF.

    IF i_max_tokens IS NOT INITIAL.
      me->_max_tokens = i_max_tokens.
    ENDIF.

    me->_temperature = 1.

    me->_max_tools_calls = 5.

    IF i_o_connection IS SUPPLIED.
      me->_o_connection = i_o_connection.
    ENDIF.

    IF i_o_persistence IS SUPPLIED.

      me->_o_persistence = i_o_persistence.

      me->_o_persistence->get_chat(
        IMPORTING
          e_t_msg_data = me->_chat_messages
      ).

    ENDIF.

  ENDMETHOD.


  METHOD get_instance.

    IF m_ref IS NOT BOUND.
      m_ref = NEW #( ).
    ENDIF.

    IF i_model IS SUPPLIED.
      m_ref->set_model( i_model ).
    ENDIF.

    r_ref = m_ref.

  ENDMETHOD.


  METHOD yif_aaic_anthropic~bind_tools.

    me->mo_function_calling = i_o_function_calling.

    IF i_max_tools_calls IS SUPPLIED.
      me->_max_tools_calls = i_max_tools_calls.
    ENDIF.

  ENDMETHOD.


  METHOD yif_aaic_anthropic~chat.

    FIELD-SYMBOLS <l_data> TYPE string.

    DATA lr_data TYPE REF TO data.

    DATA lt_response_content TYPE yif_aaic_anthropic~ty_content_t.

    DATA ls_anthropic_chat_response TYPE yif_aaic_anthropic~ty_anthropic_chat_response_s.

    DATA: l_message TYPE string,
          l_prompt  TYPE string,
          l_tools   TYPE string VALUE '[]'.

    CLEAR: e_response,
           e_failed.

    FREE e_t_response.

    IF me->_model IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_aaic_util) = NEW ycl_aaic_util( ).

    IF i_new = abap_true.

      FREE me->_chat_messages.

    ENDIF.

    IF me->_chat_messages IS INITIAL.

      IF i_greeting IS NOT INITIAL.

        APPEND INITIAL LINE TO me->_chat_messages ASSIGNING FIELD-SYMBOL(<ls_msg>).

        <ls_msg> = VALUE #( role = 'assistant' content = lo_aaic_util->serialize( i_data = i_greeting ) ).

        IF me->_o_persistence IS BOUND.
          me->_o_persistence->persist_message( i_data = <ls_msg> ).
        ENDIF.

      ENDIF.

    ENDIF.

    APPEND INITIAL LINE TO me->_chat_messages ASSIGNING <ls_msg>.

    IF i_o_prompt IS BOUND.

      l_prompt = i_o_prompt->get_prompt( ).

      l_message = i_o_prompt->get_user_message( ).

    ELSE.

      l_message = i_message.

    ENDIF.

    <ls_msg> = VALUE #( role = 'user' content = l_message ).

    IF l_prompt IS NOT INITIAL.

      DATA(ls_prompt) = <ls_msg>.

      ls_prompt-content = l_prompt.

    ENDIF.

    IF me->_o_persistence IS BOUND.
      " persist the user message and the augmented prompt
      me->_o_persistence->persist_message( i_data = <ls_msg>
                                           i_prompt = ls_prompt ).
    ENDIF.

    " In memory we keep the augmented prompt instead of the user message
    IF l_prompt IS NOT INITIAL.
      <ls_msg>-content = l_prompt.
    ENDIF.

    IF me->_o_connection IS NOT BOUND.

      me->_o_connection = NEW ycl_aaic_conn( i_api = yif_aaic_const=>c_anthropic ).

    ENDIF.

    me->_o_connection->add_http_header_param(
        EXPORTING
          i_name  = 'anthropic-version'
          i_value = me->m_anthropic_version
      ).

    me->_o_connection->add_http_header_param(
      EXPORTING
        i_name  = 'x-api-key'
        i_value = |{ yif_aaic_const=>c_placeholder_pattern }APIKEY{ yif_aaic_const=>c_placeholder_pattern }|
    ).

    IF me->m_endpoint IS INITIAL.
      me->m_endpoint = yif_aaic_const=>c_anthropic_messages_endpoint.
    ENDIF.

    IF i_o_agent IS BOUND AND me->mo_function_calling IS NOT BOUND.

      me->mo_function_calling = NEW ycl_aaic_func_call_anthropic( i_o_agent ).

    ENDIF.

    DO me->_max_tools_calls TIMES.

      IF me->_o_connection->create( i_endpoint = me->m_endpoint ).

        FREE ls_anthropic_chat_response.

        IF me->mo_function_calling IS BOUND.

          me->mo_function_calling->get_tools(
            IMPORTING
              e_tools = l_tools
          ).

        ENDIF.

        DATA(l_json) = lo_aaic_util->serialize( i_data = VALUE yif_aaic_anthropic~ty_anthropic_chat_request_s( model = me->_model
                                                                                                               temperature = me->_temperature
                                                                                                               max_tokens = me->_max_tokens
                                                                                                               stream = abap_false
                                                                                                               system = me->_system_instructions
                                                                                                               messages = me->_chat_messages
                                                                                                               tools = l_tools ) ).

        me->_o_connection->set_body( l_json ).

        FREE l_json.

        me->_o_connection->execute(
          IMPORTING
            e_response = l_json
            e_failed   = e_failed
        ).

        IF e_failed = abap_true.

          me->_o_connection->get_error_text(
            IMPORTING
              e_error_text = e_response
          ).

          IF e_t_response IS REQUESTED.
            APPEND INITIAL LINE TO e_t_response ASSIGNING FIELD-SYMBOL(<l_response>).
            <l_response> = e_response.
          ENDIF.

          EXIT.

        ENDIF.

        lo_aaic_util->deserialize(
          EXPORTING
            i_json = l_json
          IMPORTING
            e_data = ls_anthropic_chat_response
        ).

        IF ls_anthropic_chat_response-type = 'error'.

          e_response = ls_anthropic_chat_response-error-message.

          IF e_t_response IS REQUESTED.
            APPEND INITIAL LINE TO e_t_response ASSIGNING <l_response>.
            <l_response> = e_response.
          ENDIF.

          EXIT.

        ENDIF.

        APPEND INITIAL LINE TO me->_chat_messages ASSIGNING <ls_msg>.

        <ls_msg> = VALUE #( role = ls_anthropic_chat_response-role
                            content = ls_anthropic_chat_response-content ).

        IF me->_o_persistence IS BOUND.
          me->_o_persistence->persist_message( i_data = <ls_msg> ).
        ENDIF.

        lo_aaic_util->deserialize(
          EXPORTING
            i_json = ls_anthropic_chat_response-content
          IMPORTING
            e_data = lt_response_content
        ).

        IF ls_anthropic_chat_response-stop_reason = 'tool_use' AND me->mo_function_calling IS BOUND.

          LOOP AT lt_response_content ASSIGNING FIELD-SYMBOL(<ls_content>).

            IF <ls_content>-type = 'text'.
              e_response = |{ e_response }{ cl_abap_char_utilities=>newline }{ <ls_content>-text }|.
            ENDIF.

            IF <ls_content>-type <> 'tool_use'.
              CONTINUE.
            ENDIF.

            me->mo_function_calling->call_tool(
              EXPORTING
                i_tool_name   = to_upper( <ls_content>-name )
                i_json        = <ls_content>-input
              RECEIVING
                r_response    = DATA(l_tool_response)
            ).

            l_tool_response = lo_aaic_util->serialize(
              EXPORTING
                i_data = l_tool_response
            ).

            l_tool_response = '[{"type": "tool_result", "tool_use_id": "' && <ls_content>-id && '","content": ' && l_tool_response && '}]'.

            APPEND INITIAL LINE TO me->_chat_messages ASSIGNING <ls_msg>.

            <ls_msg> = VALUE #( role = 'user' content = l_tool_response ).

            IF me->_o_persistence IS BOUND.
              me->_o_persistence->persist_message( i_data = <ls_msg> ).
            ENDIF.

          ENDLOOP.

          CONTINUE.

        ENDIF.

        LOOP AT lt_response_content ASSIGNING <ls_content>.

          CASE <ls_content>-type.

            WHEN 'text'.

              e_response = |{ e_response }{ cl_abap_char_utilities=>newline }{ <ls_content>-text }|.

          ENDCASE.

        ENDLOOP.

        EXIT.

      ELSE.

        me->_o_connection->get_error_text(
          IMPORTING
            e_error_text = e_response
        ).

        IF e_t_response IS REQUESTED.
          APPEND INITIAL LINE TO e_t_response ASSIGNING <l_response>.
          <l_response> = e_response.
        ENDIF.

        EXIT.

      ENDIF.

    ENDDO.

    IF e_t_response IS REQUESTED AND ls_anthropic_chat_response-type <> 'error'.

      SPLIT e_response AT cl_abap_char_utilities=>newline INTO TABLE e_t_response.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aaic_anthropic~get_conversation.

    rt_messages = me->_chat_messages.

  ENDMETHOD.


  METHOD yif_aaic_anthropic~get_history.

    e_t_history = me->_chat_messages.

  ENDMETHOD.


  METHOD yif_aaic_anthropic~set_connection.

    me->_o_connection = i_o_connection.

  ENDMETHOD.


  METHOD yif_aaic_anthropic~set_history.

    me->_chat_messages = i_t_history.

  ENDMETHOD.


  METHOD yif_aaic_anthropic~set_max_tokens.

    me->_max_tokens = i_max_tokens.

  ENDMETHOD.


  METHOD yif_aaic_anthropic~set_model.

    me->_model = i_model.

  ENDMETHOD.


  METHOD yif_aaic_anthropic~set_persistence.

    me->_o_persistence = i_o_persistence.

  ENDMETHOD.


  METHOD yif_aaic_anthropic~set_system_instructions.

    me->_system_instructions = i_system_instructions.

  ENDMETHOD.


  METHOD yif_aaic_anthropic~set_temperature.

    me->_temperature = i_temperature.

  ENDMETHOD.


  METHOD yif_aaic_anthropic~set_version.

    me->m_anthropic_version = i_version.

  ENDMETHOD.


  METHOD yif_aaic_chat~chat.

    me->chat(
      EXPORTING
        i_message    = i_message
        i_new        = i_new
        i_greeting   = i_greeting
        i_o_prompt   = i_o_prompt
        i_o_agent    = i_o_agent
      IMPORTING
        e_response   = e_response
        e_failed     = e_failed
        e_t_response = e_t_response
    ).

  ENDMETHOD.

  METHOD yif_aaic_anthropic~set_endpoint.

    m_endpoint = i_endpoint.

  ENDMETHOD.

ENDCLASS.
