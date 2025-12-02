
CLASS ycl_aaic_db DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES yif_aaic_db.

    ALIASES create_id FOR yif_aaic_db~create_id.
    ALIASES delete_chat FOR yif_aaic_db~delete_chat.
    ALIASES persist_chat FOR yif_aaic_db~persist_chat.
    ALIASES persist_message FOR yif_aaic_db~persist_message.
    ALIASES persist_system_instructions FOR yif_aaic_db~persist_system_instructions.
    ALIASES persist_tools FOR yif_aaic_db~persist_tools.
    ALIASES get_chat FOR yif_aaic_db~get_chat.
    ALIASES block_chat FOR yif_aaic_db~block_chat.
    ALIASES release_chat FOR yif_aaic_db~release_chat.

    ALIASES mt_messages FOR yif_aaic_db~mt_messages.
    ALIASES mt_tools FOR yif_aaic_db~mt_tools.

    DATA: m_api  TYPE string READ-ONLY,
          m_id   TYPE uuid READ-ONLY,
          m_user TYPE string.


    METHODS constructor
      IMPORTING
        i_api     TYPE csequence
        i_id      TYPE uuid OPTIONAL
        i_preload TYPE abap_bool DEFAULT abap_false.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aaic_db IMPLEMENTATION.


  METHOD constructor.

    me->m_api = i_api.

    me->m_user = cl_abap_context_info=>get_user_technical_name( ).

    IF i_id IS NOT INITIAL.

      me->m_id = i_id.

      IF i_preload = abap_true.

        me->get_chat(
          EXPORTING
            i_id         = me->m_id
          IMPORTING
            e_t_messages = me->mt_messages
            e_t_tools    = me->mt_tools
        ).

      ENDIF.

    ENDIF.

    IF me->m_id IS INITIAL.

      me->persist_chat(
        IMPORTING
          e_id = me->m_id
      ).

    ENDIF.

  ENDMETHOD.


  METHOD yif_aaic_db~create_id.

    r_id = xco_cp=>uuid( )->value.

  ENDMETHOD.


  METHOD yif_aaic_db~delete_chat.

    DELETE FROM yaaic_chat WHERE id = @me->m_id.

    e_deleted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.


  METHOD yif_aaic_db~get_chat.

    DATA l_id TYPE uuid.

    FREE: e_t_messages,
          e_t_msg_data,
          e_t_tools.

    IF i_id IS SUPPLIED.
      l_id = i_id.
    ENDIF.

    IF l_id IS INITIAL.
      l_id = me->m_id.
    ENDIF.

    SELECT SINGLE id
      FROM yaaic_chat
      WHERE id = @l_id
        AND api = @me->m_api
      INTO @l_id.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT id, seqno, msg, msg_date, msg_time
      FROM yaaic_msg
      WHERE id = @l_id
      ORDER BY PRIMARY KEY
      INTO CORRESPONDING FIELDS OF TABLE @e_t_messages.

    IF i_ui = abap_false.

      SELECT id, seqno, prompt
        FROM yaaic_prompt
        WHERE id = @l_id
        ORDER BY PRIMARY KEY
        INTO TABLE @DATA(lt_prompt).

      LOOP AT lt_prompt ASSIGNING FIELD-SYMBOL(<ls_prompt>).

        READ TABLE e_t_messages ASSIGNING FIELD-SYMBOL(<ls_message>)
          WITH KEY id = <ls_prompt>-id
                   seqno = <ls_prompt>-seqno
          BINARY SEARCH.

        IF sy-subrc = 0.

          <ls_message>-msg = <ls_prompt>-prompt.

        ENDIF.

      ENDLOOP.

    ENDIF.

    SELECT id, class_name, method_name, proxy_class, description
      FROM yaaic_tools
      WHERE id = @i_id
      INTO CORRESPONDING FIELDS OF TABLE @e_t_tools.

    IF e_t_msg_data IS REQUESTED.

      DATA(lo_aaic_util) = NEW ycl_aaic_util( ).

      LOOP AT e_t_messages ASSIGNING FIELD-SYMBOL(<l_msg>).

        APPEND INITIAL LINE TO e_t_msg_data ASSIGNING FIELD-SYMBOL(<ls_msg>).

        lo_aaic_util->deserialize(
          EXPORTING
            i_json = <l_msg>-msg
          IMPORTING
            e_data = <ls_msg>
        ).

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aaic_db~persist_chat.

    DATA l_id TYPE uuid.

    CLEAR: e_id,
           e_persisted.

    IF i_id IS SUPPLIED.
      l_id = i_id.
    ENDIF.

    IF l_id IS INITIAL.
      l_id = me->create_id( ).
    ENDIF.

    DATA(ls_chat) = VALUE yaaic_chat( id = l_id
                                      api = me->m_api
                                      username = me->m_user
                                      chat_date = cl_abap_context_info=>get_system_date( )
                                      chat_time = cl_abap_context_info=>get_system_time( ) ).

    INSERT yaaic_chat FROM @ls_chat.

    e_persisted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

    e_id = l_id.

  ENDMETHOD.


  METHOD yif_aaic_db~persist_message.

    DATA: l_id    TYPE uuid,
          l_seqno TYPE i.

    CLEAR: e_id,
           e_persisted.

    IF i_id IS SUPPLIED.

      l_id = i_id.

    ENDIF.

    IF l_id IS INITIAL.

      l_id = me->m_id.

    ENDIF.

    IF l_id IS INITIAL.

      me->persist_chat(
        IMPORTING
          e_id = me->m_id
      ).

      l_id = me->m_id.

      l_seqno = 1.

    ENDIF.

    e_id = l_id.

    IF l_seqno = 0.

      SELECT MAX( seqno )
        FROM yaaic_msg
        WHERE id = @l_id
        INTO @l_seqno.

      l_seqno += 1.

    ENDIF.

    IF i_message IS SUPPLIED.

      DATA(ls_msg) = VALUE yaaic_msg( id = l_id
                                      seqno = l_seqno
                                      msg = i_message ).

    ENDIF.

    IF i_data IS SUPPLIED.

      DATA(lo_aaic_util) = NEW ycl_aaic_util( ).

      ls_msg = VALUE yaaic_msg( id = l_id
                                seqno = l_seqno
                                msg = lo_aaic_util->serialize( i_data ) ).

    ENDIF.

    IF i_prompt IS SUPPLIED AND i_prompt IS NOT INITIAL.

      DATA(ls_prompt) = VALUE yaaic_prompt( id = l_id
                                            seqno = l_seqno
                                            prompt = lo_aaic_util->serialize( i_prompt ) ).

    ENDIF.

    IF ls_msg IS NOT INITIAL.

      ls_msg-msg_date = cl_abap_context_info=>get_system_date( ).
      ls_msg-msg_time = cl_abap_context_info=>get_system_time( ).

      INSERT yaaic_msg FROM @ls_msg.

      e_persisted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

      IF ls_prompt IS NOT INITIAL.

        INSERT yaaic_prompt FROM @ls_prompt.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aaic_db~persist_system_instructions.

    DATA: l_id    TYPE uuid,
          l_seqno TYPE i.

    CLEAR: e_id,
           e_persisted.

    IF i_id IS SUPPLIED.

      l_id = i_id.

    ENDIF.

    IF l_id IS INITIAL.

      l_id = me->m_id.

    ENDIF.

    IF l_id IS INITIAL.

      me->persist_chat(
        IMPORTING
          e_id = me->m_id
      ).

      l_id = me->m_id.

    ENDIF.

    e_id = l_id.

    l_seqno = 0.

    IF i_system_instructions IS SUPPLIED.

      DATA(ls_msg) = VALUE yaaic_msg( id = l_id
                                      seqno = l_seqno
                                      msg = i_system_instructions ).

    ENDIF.

    IF i_data IS SUPPLIED.

      DATA(lo_aaic_util) = NEW ycl_aaic_util( ).

      ls_msg = VALUE yaaic_msg( id = l_id
                                seqno = l_seqno
                                msg = lo_aaic_util->serialize( i_data ) ).

    ENDIF.

    IF ls_msg IS NOT INITIAL.

      ls_msg-msg_date = cl_abap_context_info=>get_system_date( ).
      ls_msg-msg_time = cl_abap_context_info=>get_system_time( ).

      INSERT yaaic_msg FROM @ls_msg.

      e_persisted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

    ENDIF.

  ENDMETHOD.


  METHOD yif_aaic_db~persist_tools.

    IF i_t_tools[] IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_tools) = i_t_tools.

    LOOP AT lt_tools ASSIGNING FIELD-SYMBOL(<ls_tool>).

      <ls_tool>-id = me->m_id.

    ENDLOOP.

    INSERT yaaic_tools FROM TABLE @lt_tools ACCEPTING DUPLICATE KEYS.

    e_persisted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.

  METHOD yif_aaic_db~block_chat.

    UPDATE yaaic_chat
      SET blocked = @abap_true
      WHERE id = @me->m_id.

    e_blocked = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.

  METHOD yif_aaic_db~release_chat.

    UPDATE yaaic_chat
      SET blocked = @abap_false
      WHERE id = @me->m_id.

    e_released = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.
ENDCLASS.
