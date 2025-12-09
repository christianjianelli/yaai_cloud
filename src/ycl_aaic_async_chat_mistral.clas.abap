CLASS ycl_aaic_async_chat_mistral DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_serializable_object.
    INTERFACES if_bgmc_operation.
    INTERFACES if_bgmc_op_single_tx_uncontr.

    METHODS constructor
      IMPORTING
        i_task_id  TYPE yaaic_async-id
        i_chat_id  TYPE yaaic_chat-id
        i_api_key  TYPE csequence
        i_message  TYPE csequence
        i_context  TYPE csequence OPTIONAL
        i_agent_id TYPE csequence OPTIONAL
        i_model    TYPE csequence OPTIONAL
        i_log      TYPE abap_bool DEFAULT abap_true.

    METHODS on_http_request_send FOR EVENT on_request_send OF ycl_aaic_conn.

    METHODS on_http_response_received FOR EVENT on_response_received OF ycl_aaic_conn.

    METHODS on_connection_error FOR EVENT on_connection_error OF ycl_aaic_conn.

    METHODS on_message_send FOR EVENT on_message_send OF ycl_aaic_openai.

    METHODS on_response_received FOR EVENT on_response_received OF ycl_aaic_openai.

    METHODS on_message_failed FOR EVENT on_message_failed OF ycl_aaic_openai
      IMPORTING
        error_text.

    METHODS on_tool_call FOR EVENT on_tool_call OF yif_aaic_func_call_openai
      IMPORTING
        class_name
        method_name.

    METHODS on_tool_call_response FOR EVENT on_tool_call_response OF yif_aaic_func_call_openai
      IMPORTING
        tool_response.

    METHODS on_tool_call_error FOR EVENT on_tool_call_error OF yif_aaic_func_call_openai
      IMPORTING
        error_text.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _task_id  TYPE string,
          _chat_id  TYPE string,
          _agent_id TYPE string,
          _model    TYPE string,
          _api_key  TYPE string,
          _message  TYPE string,
          _context  TYPE string,
          _response TYPE string,
          _log      TYPE abap_bool.

ENDCLASS.



CLASS ycl_aaic_async_chat_mistral IMPLEMENTATION.

  METHOD constructor.

    me->_task_id  = i_task_id.
    me->_chat_id  = i_chat_id.
    me->_api_key  = i_api_key.
    me->_message  = i_message.
    me->_context  = i_context.
    me->_agent_id = i_agent_id.
    me->_model    = i_model.
    me->_log      = i_log.

  ENDMETHOD.

  METHOD if_bgmc_op_single_tx_uncontr~execute.

    DATA lo_agent TYPE REF TO yif_aaic_agent.

    DATA(lo_aaic_conn) = NEW ycl_aaic_conn( i_api = yif_aaic_const=>c_mistral ).

    lo_aaic_conn->set_api_key( i_api_key = me->_api_key ).

    DATA(lo_aaic_db) = NEW ycl_aaic_db( i_api = yif_aaic_const=>c_mistral
                                        i_id = CONV #( me->_chat_id ) ).

    IF me->_agent_id IS NOT INITIAL.

      lo_agent = NEW ycl_aaic_agent(
        i_agent_id = CONV #( me->_agent_id )
        i_chat_id  = lo_aaic_db->m_id
      ).

    ENDIF.

    DATA(lo_aaic_mistral) = NEW ycl_aaic_openai( i_model = me->_model
                                                 i_o_connection = lo_aaic_conn
                                                 i_o_persistence = lo_aaic_db
                                                 i_o_agent = lo_agent ).

    lo_aaic_mistral->use_completions( ).

    IF me->_context IS INITIAL.

      lo_aaic_mistral->chat(
        EXPORTING
          i_message       = me->_message
          i_async_task_id = me->_task_id
          i_o_agent       = lo_agent
        IMPORTING
          e_response      = me->_response
      ).

    ELSE.

      " Default template
      DATA(l_prompt_template) = |**User message**: %USER_MESSAGE% \n\n**Context**:\n\n %CONTEXT% \n\n|.

      IF me->_agent_id IS NOT INITIAL.

        l_prompt_template = lo_agent->get_prompt_template( CONV #( me->_agent_id ) ).

      ENDIF.

      DATA(lo_aaic_prompt_template) = NEW ycl_aaic_prompt_template(
        i_template_text = l_prompt_template
      ).

      DATA(lo_aaic_prompt) = NEW ycl_aaic_prompt(
        i_o_prompt_template = lo_aaic_prompt_template
        i_s_params = VALUE yif_aaic_prompt=>ty_params_basic_s( user_message = me->_message
                                                               context = me->_context )
      ).

      lo_aaic_mistral->chat(
        EXPORTING
          i_o_prompt = lo_aaic_prompt
          i_o_agent  = lo_agent
        IMPORTING
          e_response = me->_response
      ).

    ENDIF.

    DATA(lo_async) = NEW ycl_aaic_async( ).

    lo_async->update_status(
      EXPORTING
        i_task_id = CONV #( me->_task_id )
        i_status  = yif_aaic_async=>mc_task_finished
    ).

  ENDMETHOD.

  METHOD on_message_send.

    IF me->_log = abap_true.

      DATA(lo_log) = NEW ycl_aaic_log( CONV #( me->_chat_id ) ).

      lo_log->add( VALUE #( number = '005' type = 'S' ) ).

    ENDIF.

  ENDMETHOD.

  METHOD on_response_received.

    IF me->_log = abap_true.

      DATA(lo_log) = NEW ycl_aaic_log( CONV #( me->_chat_id ) ).

      lo_log->add( VALUE #( number = '006' type = 'S' ) ).

    ENDIF.

  ENDMETHOD.

  METHOD on_message_failed.

    IF me->_log = abap_true.

      DATA(lo_log) = NEW ycl_aaic_log( CONV #( me->_chat_id ) ).

      lo_log->add( VALUE #( number = '007' type = 'E' ) ).

    ENDIF.

  ENDMETHOD.

  METHOD on_tool_call.

    IF me->_log = abap_true.

      DATA(lo_log) = NEW ycl_aaic_log( CONV #( me->_chat_id ) ).

      lo_log->add( VALUE #( number = '008'
                                type = 'S'
                                message_v1 = class_name
                                message_v2 = method_name ) ).

    ENDIF.

  ENDMETHOD.

  METHOD on_tool_call_response.

    IF me->_log = abap_true.

      DATA(lo_log) = NEW ycl_aaic_log( CONV #( me->_chat_id ) ).

      lo_log->add( VALUE #( number = '009' type = 'S' message_v1 = tool_response ) ).

    ENDIF.

  ENDMETHOD.

  METHOD on_tool_call_error.

    IF me->_log = abap_true.

      DATA(lo_log) = NEW ycl_aaic_log( CONV #( me->_chat_id ) ).

      lo_log->add( VALUE #( number = '010' type = 'E' message_v1 = error_text ) ).

    ENDIF.

  ENDMETHOD.

  METHOD on_connection_error.

    IF me->_log = abap_true.

      DATA(lo_log) = NEW ycl_aaic_log( CONV #( me->_chat_id ) ).

      lo_log->add( VALUE #( number = '001' type = 'E' ) ).

    ENDIF.

  ENDMETHOD.

  METHOD on_http_request_send.

    DATA(lo_log) = NEW ycl_aaic_log( CONV #( me->_chat_id ) ).

    lo_log->add( VALUE #( number = '011' type = 'S' ) ).

  ENDMETHOD.

  METHOD on_http_response_received.

    IF me->_log = abap_true.

      DATA(lo_log) = NEW ycl_aaic_log( CONV #( me->_chat_id ) ).

      lo_log->add( VALUE #( number = '012' type = 'S' ) ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.
