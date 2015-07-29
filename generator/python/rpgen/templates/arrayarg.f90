    {% macro dimension(n) -%}
    {% if ndim != 0 -%}
    , DIMENSION({{ ([':'] * n)|join(', ') }})
    {%- endif %}
    {%- endmacro %}
    {% macro dimension_full(n) -%}
    {% if ndim != 0 -%}
    , DIMENSION({% for i in range(n) -%}SIZE(a, {{ i + 1 }}){% if loop.last %}{% else %}, {% endif %}{%- endfor %})
    {%- endif %}
    {%- endmacro %}
    FUNCTION {{ function.name }}_{{ type1.name }}_{{ ndim }}d (a) RESULT (x)
        {{ type1.declaration }}{{ dimension(ndim) }}, INTENT(IN) :: a
        {{ function.return_type.declaration }} :: x
        REAL(KIND=RPE_REAL_KIND){{ dimension_full(ndim) }} :: t
        t = a
        x = {{ function.name.upper() }}(t)
    END FUNCTION {{ function.name }}_{{ type1.name }}_{{ ndim }}d
