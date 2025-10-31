CLASS ycl_aaic_async DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES yif_aaic_async.

    ALIASES create FOR yif_aaic_async~create.
    ALIASES read FOR yif_aaic_async~read.
    ALIASES update FOR yif_aaic_async~update.
    ALIASES delete FOR yif_aaic_async~delete.
    ALIASES run FOR yif_aaic_async~run.
    ALIASES update_status FOR yif_aaic_async~update_status.
    ALIASES get_status FOR yif_aaic_async~get_status.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aaic_async IMPLEMENTATION.

  METHOD yif_aaic_async~create.

    CLEAR r_task_id.

    DATA(ls_task) = VALUE yaaic_async( id = xco_cp=>uuid( )->value
                                       chat_id = i_chat_id
                                       status = yif_aaic_async~mc_task_created
                                       username = xco_cp=>sy->user( )->name
                                       startdate = xco_cp=>sy->date( )->as( xco_cp_time=>format->abap )->value
                                       starttime = xco_cp=>sy->time( )->as( xco_cp_time=>format->abap )->value
    ).

    INSERT yaaic_async FROM @ls_task.

    IF sy-subrc = 0.
      r_task_id = ls_task-id.
    ENDIF.

  ENDMETHOD.

  METHOD yif_aaic_async~read.

    SELECT SINGLE id, chat_id, status, username, startdate, starttime
      FROM yaaic_async
      WHERE id = @i_task_id
      INTO CORRESPONDING FIELDS OF @e_s_task.

  ENDMETHOD.

  METHOD yif_aaic_async~update.

    r_updated = abap_false.

    DATA(ls_task) = CORRESPONDING yaaic_async( i_s_task ).

    UPDATE yaaic_async FROM @ls_task.

    IF sy-subrc = 0.

      r_updated = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aaic_async~delete.

    r_deleted = abap_false.

    DELETE FROM yaaic_async WHERE id = @i_task_id.

    IF sy-subrc = 0.

      r_deleted = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aaic_async~run.

    DATA lo_background_process TYPE REF TO if_bgmc_process_single_op.

    r_started = abap_false.

    TRY.

        lo_background_process = cl_bgmc_process_factory=>get_default(  )->create(  ).
        lo_background_process->set_operation_tx_uncontrolled( i_o_task ).
        lo_background_process->save_for_execution(  ).

      CATCH cx_bgmc INTO DATA(exception) ##NO_HANDLER.
        RETURN.
    ENDTRY.

    IF me->update_status(
      EXPORTING
        i_task_id = i_task_id
        i_status  = yif_aaic_async~mc_task_running ).

      COMMIT WORK.

      r_started = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aaic_async~update_status.

    r_updated = abap_false.

    UPDATE yaaic_async SET status = @i_status WHERE id = @i_task_id.

    IF sy-subrc = 0.

      r_updated = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aaic_async~get_status.

    CLEAR r_status.

    SELECT SINGLE status
      FROM yaaic_async
      WHERE id = @i_task_id
      INTO @r_status.

  ENDMETHOD.

  METHOD yif_aaic_async~get_tasks_by_chat_id.

    SELECT id, chat_id, status, username, startdate, starttime
      FROM yaaic_async
      WHERE chat_id = @i_chat_id
      INTO CORRESPONDING FIELDS OF TABLE @r_t_tasks.

  ENDMETHOD.

ENDCLASS.
