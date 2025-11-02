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

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aaic_agent IMPLEMENTATION.

  METHOD yif_aaic_agent~get_system_instructions.

    DATA ls_agent TYPE yaaic_agent.

    FREE r_system_instructions.

    IF i_agent_id IS SUPPLIED.

      SELECT SINGLE id, name, sys_inst_id
        FROM yaaic_agent
        WHERE id = @i_agent_id
        INTO CORRESPONDING FIELDS OF @ls_agent.

    ELSEIF i_agent_name IS SUPPLIED.

      SELECT id, name, sys_inst_id
        FROM yaaic_agent
        WHERE name = @i_agent_name
        INTO CORRESPONDING FIELDS OF @ls_agent
        UP TO 1 ROWS.
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

      SELECT a~id, a~name, b~class_name, b~method_name, b~proxy_class, b~description
        FROM yaaic_agent AS a
        INNER JOIN yaaic_agent_tool AS b
        ON a~id = b~id
        WHERE a~id = @i_agent_id
        INTO TABLE @DATA(lt_tools).

    ELSEIF i_agent_name IS SUPPLIED.

      SELECT a~id, a~name, b~class_name, b~method_name, b~proxy_class, b~description
        FROM yaaic_agent AS a
        INNER JOIN yaaic_agent_tool AS b
        ON a~id = b~id
        WHERE a~name = @i_agent_name
        INTO TABLE @lt_tools.

    ENDIF.

    r_t_agent_tools = CORRESPONDING #( lt_tools ).

  ENDMETHOD.

  METHOD yif_aaic_agent~get_prompt_template.

    DATA ls_agent TYPE yaaic_agent.

    FREE r_prompt_template.

    IF i_agent_id IS SUPPLIED.

      SELECT SINGLE id, name, prompt_template
        FROM yaaic_agent
        WHERE id = @i_agent_id
        INTO CORRESPONDING FIELDS OF @ls_agent.

    ELSEIF i_agent_name IS SUPPLIED.

      SELECT id, name, prompt_template
        FROM yaaic_agent
        WHERE name = @i_agent_name
        INTO CORRESPONDING FIELDS OF @ls_agent
        UP TO 1 ROWS.
      ENDSELECT.

    ENDIF.

    r_prompt_template = ls_agent-prompt_template.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

*    DATA(l_agent_id) = '7EA3422BA1AC1FE0AE835528E8F90C68'.
*
*    DATA(l_system_instructions) = me->get_system_instructions( CONV #( l_agent_id ) ).
*
*    DATA(lt_tools) = me->get_tools( CONV #( l_agent_id ) ).
*
*    DATA(l_prompt_template) = me->get_prompt_template( CONV #( l_agent_id ) ).
*
*    out->write( l_system_instructions ).
*
*    out->write( lt_tools ).
*
*    out->write( l_prompt_template ).

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

  ENDMETHOD.

ENDCLASS.
