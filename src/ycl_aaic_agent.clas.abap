CLASS ycl_aaic_agent DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aaic_agent.

    INTERFACES if_oo_adt_classrun.

    ALIASES get_system_instructions FOR yif_aaic_agent~get_system_instructions.
    ALIASES get_tools FOR yif_aaic_agent~get_tools.
    ALIASES get_prompt_template FOR yif_aaic_agent~get_prompt_template.
    ALIASES m_agent_id FOR yif_aaic_agent~m_agent_id.
    ALIASES m_chat_id FOR yif_aaic_agent~m_chat_id.

    METHODS constructor
      IMPORTING
        i_agent_id TYPE yaaic_agent-id OPTIONAL
        i_chat_id  TYPE yaaic_chat-id OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aaic_agent IMPLEMENTATION.

  METHOD constructor.

    me->m_agent_id = i_agent_id.
    me->m_chat_id = i_chat_id.

  ENDMETHOD.

  METHOD yif_aaic_agent~get_system_instructions.

    DATA ls_agent TYPE yaaic_agent.

    FREE r_system_instructions.

    IF i_agent_id IS SUPPLIED.
      me->m_agent_id = i_agent_id.
    ENDIF.

    IF me->m_agent_id IS NOT INITIAL.

      SELECT SINGLE id, name, sys_inst_id
        FROM yaaic_agent
        WHERE id = @me->m_agent_id
        INTO CORRESPONDING FIELDS OF @ls_agent.

    ELSEIF i_agent_name IS SUPPLIED.

      SELECT id, name, sys_inst_id
        FROM yaaic_agent
        WHERE name = @i_agent_name
        INTO CORRESPONDING FIELDS OF @ls_agent
        UP TO 1 ROWS.                                   "#EC CI_NOORDER
      ENDSELECT.

    ENDIF.

    IF ls_agent-sys_inst_id IS INITIAL.
      RETURN.
    ENDIF.

    NEW ycl_aaic_rag_db( )->read(
      EXPORTING
        i_id      = ls_agent-sys_inst_id
      IMPORTING
        e_content = r_system_instructions
    ).

  ENDMETHOD.

  METHOD yif_aaic_agent~get_tools.

    FREE r_t_agent_tools.

    IF i_agent_id IS SUPPLIED.
      me->m_agent_id = i_agent_id.
    ENDIF.

    IF i_chat_id IS SUPPLIED.
      me->m_chat_id = i_chat_id.
    ENDIF.

    IF me->m_agent_id IS NOT INITIAL.

      SELECT a~id, a~name, b~class_name, b~method_name, b~proxy_class, b~description
        FROM yaaic_agent AS a
        INNER JOIN yaaic_agent_tool AS b
        ON a~id = b~id
        WHERE a~id = @me->m_agent_id
          AND load_on_demand = @i_load_on_demand_tools
        INTO TABLE @DATA(lt_tools).

    ELSEIF i_agent_name IS SUPPLIED.

      SELECT a~id, a~name, b~class_name, b~method_name, b~proxy_class, b~description
        FROM yaaic_agent AS a
        INNER JOIN yaaic_agent_tool AS b
        ON a~id = b~id
        WHERE a~name = @i_agent_name
        AND load_on_demand = @i_load_on_demand_tools
        INTO TABLE @lt_tools.                           "#EC CI_NOORDER

      IF sy-subrc = 0.
        me->m_agent_id = lt_tools[ 1 ]-id.              "#EC CI_NOORDER
      ENDIF.

    ENDIF.

    IF i_load_on_demand_tools = abap_false AND
       me->m_chat_id IS NOT INITIAL AND
       me->m_agent_id IS NOT INITIAL.

      SELECT a~id, b~class_name, b~method_name, b~proxy_class, b~description
        FROM yaaic_tools AS a
        INNER JOIN yaaic_agent_tool AS b
        ON a~class_name = b~class_name
        AND a~method_name = b~method_name
        WHERE a~id = @me->m_chat_id
          AND b~id = @me->m_agent_id
        APPENDING CORRESPONDING FIELDS OF TABLE @lt_tools.

    ENDIF.

    r_t_agent_tools = CORRESPONDING #( lt_tools ).

  ENDMETHOD.

  METHOD yif_aaic_agent~get_prompt_template.

    DATA ls_agent TYPE yaaic_agent.

    FREE r_prompt_template.

    IF i_agent_id IS SUPPLIED.
      me->m_agent_id = i_agent_id.
    ENDIF.

    IF me->m_agent_id IS NOT INITIAL.

      SELECT SINGLE id, name, prompt_template
        FROM yaaic_agent
        WHERE id = @me->m_agent_id
        INTO CORRESPONDING FIELDS OF @ls_agent.

    ELSEIF i_agent_name IS SUPPLIED.

      SELECT id, name, prompt_template
        FROM yaaic_agent
        WHERE name = @i_agent_name
        INTO CORRESPONDING FIELDS OF @ls_agent
        UP TO 1 ROWS.                                   "#EC CI_NOORDER
      ENDSELECT.

    ENDIF.

    r_prompt_template = ls_agent-prompt_template.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

*    me->m_agent_id = '7EA3422BA1AC1FE0AE835528E8F90C68'.
*
*    DATA(l_system_instructions) = me->get_system_instructions( ).
*
*    DATA(lt_tools) = me->get_tools( ).
*
*    DATA(l_prompt_template) = me->get_prompt_template( ).
*
*    out->write( l_system_instructions ).
*
*    out->write( lt_tools ).
*
*    out->write( l_prompt_template ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

*    DATA(l_agent_name) = 'travel-fiori-ai-assistant'.
*
*    DATA(l_system_instructions) = me->get_system_instructions( i_agent_name = CONV #( l_agent_name ) ).
*
*    DATA(lt_tools) = me->get_tools( i_agent_name = CONV #( l_agent_name ) ).
*
*    DATA(l_prompt_template) = me->get_prompt_template( i_agent_name = CONV #( l_agent_name ) ).
*
*    out->write( l_system_instructions ).
*
*    out->write( lt_tools ).
*
*    out->write( l_prompt_template ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    me->m_agent_id = '7EA3422BA1AC1FE0AF9BF54114B1CC74'.
    me->m_chat_id = '2A9448FCE52F1FD0AFB19E6C3E30184D'.

    out->write( me->get_tools( ) ).



  ENDMETHOD.

ENDCLASS.
