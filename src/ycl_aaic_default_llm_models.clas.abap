CLASS ycl_aaic_default_llm_models DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_AAIC_DEFAULT_LLM_MODELS IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    DATA(ls_aaic_model) = VALUE yaaic_model( id = yif_aaic_const=>c_openai
                                             model = 'gpt-5-nano' ).

    INSERT yaaic_model FROM @ls_aaic_model.

    ls_aaic_model = VALUE #( id = yif_aaic_const=>c_anthropic
                             model = 'claude-3-7-sonnet-latest' ).

    INSERT yaaic_model FROM @ls_aaic_model.

    ls_aaic_model = VALUE #( id = yif_aaic_const=>c_google
                             model = 'gemini-2.5-flash' ).

    INSERT yaaic_model FROM @ls_aaic_model.

    ls_aaic_model = VALUE #( id = yif_aaic_const=>c_mistral
                             model = 'mistral-small-latest' ).

    INSERT yaaic_model FROM @ls_aaic_model.

    out->write( 'ABAP AI tool Cloud: Default LLM models set.' ).

  ENDMETHOD.
ENDCLASS.
