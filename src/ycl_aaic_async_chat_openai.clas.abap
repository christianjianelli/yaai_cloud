CLASS ycl_aaic_async_chat_openai DEFINITION
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
        i_model    TYPE csequence OPTIONAL.


  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _task_id  TYPE string,
          _chat_id  TYPE string,
          _agent_id TYPE string,
          _model    TYPE string,
          _api_key  TYPE string,
          _message  TYPE string,
          _context  TYPE string,
          _response TYPE string.

ENDCLASS.



CLASS ycl_aaic_async_chat_openai IMPLEMENTATION.

  METHOD constructor.

    me->_task_id  = i_task_id.
    me->_chat_id  = i_chat_id.
    me->_api_key  = i_api_key.
    me->_message  = i_message.
    me->_context  = i_context.
    me->_agent_id = i_agent_id.
    me->_model    = i_model.

  ENDMETHOD.

  METHOD if_bgmc_op_single_tx_uncontr~execute.

    DATA(lo_aaic_conn) = NEW ycl_aaic_conn( i_api = yif_aaic_const=>c_openai ).

    lo_aaic_conn->set_api_key( i_api_key = me->_api_key ).

    DATA(lo_aaic_db) = NEW ycl_aaic_db( i_api = yif_aaic_const=>c_openai
                                        i_id = CONV #( me->_chat_id ) ).


    DATA(lo_aaic_openai) = NEW ycl_aaic_openai( i_model = me->_model
                                                i_o_connection = lo_aaic_conn
                                                i_o_persistence = lo_aaic_db ).

    IF me->_agent_id IS NOT INITIAL.

      DATA(lo_agent) = NEW ycl_aaic_agent(
        i_agent_id = CONV #( me->_agent_id )
        i_chat_id  = lo_aaic_db->m_id
      ).

      DATA(l_system_instructions) = lo_agent->get_system_instructions( ).

      lo_aaic_openai->set_system_instructions(
        i_system_instructions = l_system_instructions
      ).

    ENDIF.

    IF me->_context IS INITIAL.

      lo_aaic_openai->chat(
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

      lo_aaic_openai->chat(
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

ENDCLASS.
