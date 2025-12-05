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
    ALIASES get_response FOR yif_aaic_async~get_response.
    ALIASES update_response FOR yif_aaic_async~update_response.
    ALIASES set_monitor FOR yif_aaic_async~set_monitor.
    ALIASES get_monitor FOR yif_aaic_async~get_monitor.
    ALIASES get_process_monitor FOR yif_aaic_async~get_process_monitor.
    ALIASES get_process_state FOR yif_aaic_async~get_process_state.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aaic_async IMPLEMENTATION.


  METHOD yif_aaic_async~create.

    CLEAR r_task_id.

    DATA(ls_task) = VALUE yaaic_async( id = xco_cp=>uuid( )->value
                                       chat_id = i_chat_id
                                       name = i_task_name
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

    SELECT SINGLE id, chat_id, name, status, username, startdate, starttime
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

    DATA: lo_background_process TYPE REF TO if_bgmc_process_single_op,
          lo_process_monitor    TYPE REF TO if_bgmc_process_monitor.

    r_started = abap_false.

    TRY.

        lo_background_process = cl_bgmc_process_factory=>get_default( )->create( ).
        lo_background_process->set_operation_tx_uncontrolled( i_o_task ).
        lo_process_monitor = lo_background_process->save_for_execution( ).

        DATA(l_monitor) = lo_process_monitor->to_string( ).

      CATCH cx_bgmc ##NO_HANDLER.
        RETURN.
    ENDTRY.

    IF me->update_status(
      EXPORTING
        i_task_id = i_task_id
        i_status  = yif_aaic_async~mc_task_created ).

      COMMIT WORK.

      IF lo_process_monitor IS BOUND.

        me->set_monitor(
          EXPORTING
            i_task_id = i_task_id
            i_monitor = l_monitor
        ).

        TRY.

            DATA(l_process_state) = lo_process_monitor->get_state( ).

            me->update_status(
              EXPORTING
                i_task_id       = i_task_id
                i_process_state = l_process_state ).

          CATCH cx_bgmc ##NO_HANDLER.
            RETURN.
        ENDTRY.

      ENDIF.

      r_started = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aaic_async~update_status.

    DATA: l_enddate TYPE d,
          l_endtime TYPE t,
          l_status  TYPE yaaic_async-status.

    r_updated = abap_false.

    IF i_status IS NOT INITIAL.
      l_status = i_status.
    ELSE.

      l_status  = COND #( WHEN i_process_state = if_bgmc_process_monitor=>gcs_state-erroneous THEN yif_aaic_async~mc_task_failed
                          WHEN i_process_state = if_bgmc_process_monitor=>gcs_state-unknown THEN yif_aaic_async~mc_task_failed
                          WHEN i_process_state = if_bgmc_process_monitor=>gcs_state-running THEN yif_aaic_async~mc_task_running
                          WHEN i_process_state = if_bgmc_process_monitor=>gcs_state-successful THEN yif_aaic_async~mc_task_finished ).
    ENDIF.

    IF l_status = yif_aaic_async=>mc_task_finished OR
       l_status = yif_aaic_async=>mc_task_cancelled.

      l_enddate = xco_cp=>sy->date( )->as( xco_cp_time=>format->abap )->value.
      l_endtime = xco_cp=>sy->time( )->as( xco_cp_time=>format->abap )->value.

    ENDIF.

    UPDATE yaaic_async
      SET status = @l_status,
          enddate = @l_enddate,
          endtime = @l_endtime
      WHERE id = @i_task_id.

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


  METHOD yif_aaic_async~get_response.

    CLEAR r_response.

    SELECT SINGLE response
      FROM yaaic_async
      WHERE id = @i_task_id
      INTO @r_response.

  ENDMETHOD.


  METHOD yif_aaic_async~update_response.

    r_updated = abap_false.

    UPDATE yaaic_async
      SET response = @i_response
      WHERE id = @i_task_id.

    IF sy-subrc = 0.

      r_updated = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aaic_async~get_monitor.

    CLEAR r_monitor.

    SELECT SINGLE monitor
      FROM yaaic_async
      WHERE id = @i_task_id
      INTO @r_monitor.

  ENDMETHOD.

  METHOD yif_aaic_async~set_monitor.

    r_set = abap_false.

    UPDATE yaaic_async
      SET monitor = @i_monitor
      WHERE id = @i_task_id.

    IF sy-subrc = 0.

      r_set = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aaic_async~get_process_monitor.

    CLEAR ro_process_monitor.

    DATA(l_monitor) = me->get_monitor(
      EXPORTING
        i_task_id = i_task_id
    ).

    IF l_monitor IS NOT INITIAL.

      TRY.

          ro_process_monitor = cl_bgmc_process_factory=>create_monitor_from_string( l_monitor ).

        CATCH cx_bgmc ##NO_HANDLER.

      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aaic_async~get_process_state.

    CLEAR r_state.

    TRY.

        DATA(lo_process_monitor) = me->get_process_monitor(
          EXPORTING
            i_task_id = i_task_id
        ).

        IF lo_process_monitor IS BOUND.

          r_state = lo_process_monitor->get_state( ).

        ENDIF.

      CATCH cx_bgmc ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
