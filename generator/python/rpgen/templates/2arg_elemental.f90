    ELEMENTAL FUNCTION {{ function.name }}_{{ type1.name }}_{{ type2.name }} (a, b) RESULT (x)
        {{ type1.declaration }}, INTENT(IN) :: a
        {{ type2.declaration }}, INTENT(IN) :: b
        {{ function.return_type.declaration }} :: x
        {% if function.return_type.rpe_instance %}
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        {% endif %}
        x = {{ function.name.upper() }}(a{{ type1.accessor }}, b{{ type2.accessor }})
    END FUNCTION {{ function.name }}_{{ type1.name }}_{{ type2.name }}
