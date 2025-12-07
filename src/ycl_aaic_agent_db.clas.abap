CLASS ycl_aaic_agent_db DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aaic_agent_db.
    INTERFACES if_oo_adt_classrun.

    ALIASES create FOR yif_aaic_agent_db~create.
    ALIASES read FOR yif_aaic_agent_db~read.
    ALIASES update FOR yif_aaic_agent_db~update.
    ALIASES delete FOR yif_aaic_agent_db~delete.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_AAIC_AGENT_DB IMPLEMENTATION.


  METHOD yif_aaic_agent_db~create.

    CLEAR e_id.

    SELECT id FROM yaaic_agent
      WHERE name = @i_s_agent-name
      INTO @DATA(l_id)
      UP TO 1 ROWS.
    ENDSELECT.

    IF sy-subrc = 0.
      e_error = |Agent { i_s_agent-name } already exists in the database|.
      RETURN.
    ENDIF.

    DATA(ls_agent) = i_s_agent.
    DATA(lt_agent_tools) = i_t_agent_tools.
    DATA(lt_agent_models) = i_t_agent_models.

    ls_agent-id = xco_cp=>uuid( )->value.

    LOOP AT lt_agent_tools ASSIGNING FIELD-SYMBOL(<ls_agent_tool>).
      <ls_agent_tool>-id = ls_agent-id.
    ENDLOOP.

    LOOP AT lt_agent_models ASSIGNING FIELD-SYMBOL(<ls_agent_model>).
      <ls_agent_model>-id = ls_agent-id.
    ENDLOOP.

    INSERT yaaic_agent FROM @ls_agent.

    IF sy-subrc <> 0.
      e_error = |Error while saving Agent { i_s_agent-name }|.
      RETURN.
    ENDIF.

    e_id = ls_agent-id.

    INSERT yaaic_agent_tool FROM TABLE @lt_agent_tools.

    IF sy-subrc <> 0.
      e_error = |Error while saving tools for Agent { i_s_agent-name }|.
      RETURN.
    ENDIF.

    INSERT yaaic_agent_mdl FROM TABLE @lt_agent_models.

    IF sy-subrc <> 0.
      e_error = |Error while saving models for Agent { i_s_agent-name }|.
    ENDIF.

  ENDMETHOD.


  METHOD yif_aaic_agent_db~read.

    FREE: e_error,
          e_s_agent,
          e_t_agent_tools.

    SELECT FROM yaaic_agent FIELDS id, name, description, sys_inst_id, rag_ctx_id
      WHERE id = @i_agent_id
         OR name = @i_agent_name
      INTO CORRESPONDING FIELDS OF @e_s_agent
      UP TO 1 ROWS.                                     "#EC CI_NOORDER
    ENDSELECT.

    IF sy-subrc <> 0.

      e_error = COND #( WHEN i_agent_id IS SUPPLIED
                        THEN |Agent { i_agent_id } not found in the database|
                        ELSE |Agent { i_agent_name } not found in the database| ).

      RETURN.

    ENDIF.

    SELECT FROM yaaic_agent_tool FIELDS id, class_name, method_name, proxy_class, description
      WHERE id = @e_s_agent-id
      INTO CORRESPONDING FIELDS OF TABLE @e_t_agent_tools.

  ENDMETHOD.


  METHOD yif_aaic_agent_db~update.

    e_updated = abap_false.

    SELECT SINGLE @abap_true
      FROM yaaic_agent
      WHERE id = @i_s_agent-id
      INTO @DATA(l_exist).

    IF sy-subrc <> 0.

      e_error = |Agent { i_s_agent-id } not found in the database|.

      RETURN.

    ENDIF.

    UPDATE yaaic_agent FROM @i_s_agent.

    IF sy-subrc <> 0.
      e_error = |Error while saving Agent { i_s_agent-name }|.
      RETURN.
    ENDIF.

    e_updated = abap_true.

    DELETE FROM yaaic_agent_tool WHERE id = @i_s_agent-id.
    DELETE FROM yaaic_agent_mdl WHERE id = @i_s_agent-id.

    DATA(lt_agent_tools) = i_t_agent_tools.
    DATA(lt_agent_models) = i_t_agent_models.

    LOOP AT lt_agent_tools ASSIGNING FIELD-SYMBOL(<ls_agent_tool>).
      <ls_agent_tool>-id = i_s_agent-id.
    ENDLOOP.

    LOOP AT lt_agent_models ASSIGNING FIELD-SYMBOL(<ls_agent_model>).
      <ls_agent_model>-id = i_s_agent-id.
    ENDLOOP.

    INSERT yaaic_agent_tool FROM TABLE @lt_agent_tools.

    IF sy-subrc <> 0.
      e_error = |Error while saving tools for Agent { i_s_agent-name }|.
      RETURN.
    ENDIF.

    INSERT yaaic_agent_mdl FROM TABLE @lt_agent_models.

    IF sy-subrc <> 0.
      e_error = |Error while saving models for Agent { i_s_agent-name }|.
    ENDIF.

  ENDMETHOD.


  METHOD yif_aaic_agent_db~delete.

    e_deleted = abap_false.

    DELETE FROM yaaic_agent_tool WHERE id = @i_agent_id.

    DELETE FROM yaaic_agent WHERE id = @i_agent_id.

    IF sy-subrc <> 0.
      e_error = |Error while deleting Agent { i_agent_id }|.
    ENDIF.

    e_deleted = abap_true.

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

*    DELETE FROM yaaic_agent.
*    DELETE FROM yaaic_agent_tool.

*    DATA(ls_agent) = VALUE yaaic_agent( name = 'travel-fiori-ai-assistant'
*                                        description = 'Travel Fiori App AI Assistant'
*                                        sys_inst_id = '2A9448FCE52F1FD0AD850377D753B845'
*                                        rag_ctx_id = '7EA3422BA1AC1FE0AD8503D109246C64'
*                                        prompt_template = '**User message**: %USER_MESSAGE% \n\n**Context**:\n\n %CONTEXT% \n\n' ).
*
*    DATA(lt_agent_tools) = VALUE yif_aaic_agent_db=>ty_agent_tools_t( ( class_name = 'ycl_aaic_rag_tools'
*                                                                        method_name = 'get_documentation'
*                                                                        description = 'Use this method to retrieve the complete documentation of the Travel Fiori App' ) ).
*
*    me->create(
*      EXPORTING
*        i_s_agent       = ls_agent
*        i_t_agent_tools = lt_agent_tools
*      IMPORTING
*        e_id            = DATA(l_id)
*        e_error         = DATA(l_error)
*    ).
*
*    out->write( l_id ).
*    out->write( l_error ).

  ENDMETHOD.
ENDCLASS.
