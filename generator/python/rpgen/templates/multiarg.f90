{% macro argspec() -%}
{% for i in range(types|length) -%}a{{i}}{% if loop.last %}{% else %}, {% endif %}{%- endfor %}
{%- endmacro %}

{% macro typedef() %}
{%- for i in range(types|length) -%}
    {% if i == 0 %}{{ types[i].declaration }}, INTENT(IN) :: a{{ i }}{% else %}{{ "        " + types[i].declaration }}, INTENT(IN) :: a{{ i }}{% endif %}

{% endfor -%}
{% endmacro %}

{% macro typenamelist() -%}
{% for i in range(types|length) -%}{{ types[i].name }}{% if loop.last %}{% else %}_{% endif %}{%- endfor %}
{%- endmacro %}

    ELEMENTAL FUNCTION {{ function.name }}_ma_{{ typenamelist() }} ({{ argspec() }}) RESULT (x)
        {{ typedef() }}
        {{ function.return_type.declaration }} :: x
        {% if function.return_type.rpe_instance %}
        x%sbits = MAX({% for i in range(types|length) -%}significand_bits(a{{ i }}){% if not loop.last %}, {% endif %}{%- endfor %})
        {% endif %}
        x = {{ function.name.upper() }}({% for i in range(types|length) -%}a{{ i }}{{ types[i].accessor }}{% if not loop.last %}, {% endif %}{%- endfor %})
    END FUNCTION {{ function.name }}_ma_{{ typenamelist() }}
