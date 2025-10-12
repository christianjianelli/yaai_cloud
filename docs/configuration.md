# yaai_cloud - ABAP AI Tools Cloud - Configuration

The ABAP AI Tools Cloud allows you to set the base URLs for LLM APIs and define the default LLM model to be used. These base URLs are used to construct the full API endpoint for service calls, so you don't need to set them repeatedly in every application you build. The same applies to the LLM model: you can define a default model that will always be used unless you specify a different one when creating an instance of the corresponding LLM API.

The configuration is stored in the following tables:
 - yaaic_base_url
 - yaaic_model

ABAP AI Tools Cloud includes the class `ycl_aaic_basic_setup`, which can be used to set the base URLs and default LLM models.

```abap
CLASS ycl_aaic_basic_setup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aaic_basic_setup IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    INSERT yaaic_base_url FROM TABLE @( VALUE #( ( id = 'OPENAI' base_url = 'https://api.openai.com' )
                                                 ( id = 'ANTHROPIC' base_url = 'https://api.anthropic.com' )
                                                 ( id = 'GOOGLE' base_url = 'https://generativelanguage.googleapis.com' )
                                                 ( id = 'MISTRAL' base_url = 'https://api.mistral.ai' ) ) ) ACCEPTING DUPLICATE KEYS.

    INSERT yaaic_model FROM TABLE @( VALUE #( ( id = 'OPENAI' model = 'gpt-5' )
                                              ( id = 'ANTHROPIC' model = 'claude-sonnet-4-20250514' )
                                              ( id = 'GOOGLE' model = 'gemini-2.5-flash' )
                                              ( id = 'MISTRAL' model = 'mistral-large-latest' ) ) ) ACCEPTING DUPLICATE KEYS.

  ENDMETHOD.

ENDCLASS.
```