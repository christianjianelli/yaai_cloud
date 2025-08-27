CLASS ycl_aaic_prompt_template DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aaic_prompt_template.

    ALIASES m_template_id FOR yif_aaic_prompt_template~m_template_id.
    ALIASES m_template_text FOR yif_aaic_prompt_template~m_template_text.
    ALIASES set_template_text FOR yif_aaic_prompt_template~set_template_text.

    METHODS constructor IMPORTING i_template_text TYPE clike OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.


CLASS ycl_aaic_prompt_template IMPLEMENTATION.


  METHOD constructor.

    IF i_template_text IS SUPPLIED.
      me->m_template_text = i_template_text.
    ENDIF.

  ENDMETHOD.


  METHOD yif_aaic_prompt_template~get_template_text.

    r_template_text = me->m_template_text.

  ENDMETHOD.


  METHOD yif_aaic_prompt_template~set_template_text.

    me->m_template_text = i_template_text.

  ENDMETHOD.

ENDCLASS.
