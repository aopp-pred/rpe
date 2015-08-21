    FUNCTION {{ function.name }}_{{ type1.name }} (a) RESULT (x)
        {{ type1.declaration }}, INTENT(IN) :: a
        {{ function.return_type.declaration }} :: x
        {% if function.return_type.rpe_instance %}
        x%sbits = significand_bits(a)
        {% endif %}
        x = {{ function.name.upper() }}(a{{ type1.accessor }})
    END FUNCTION {{ function.name }}_{{ type1.name }}
