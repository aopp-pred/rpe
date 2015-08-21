    ELEMENTAL FUNCTION {{ operator.name }}_{{ type1.name }}_{{ type2.name }} (x, y) RESULT (z)
        {{ type1.declaration }}, INTENT(IN) :: x
        {{ type2.declaration }}, INTENT(IN) :: y
        {{ operator.return_type.declaration }} :: z
        {% if operator.return_type.rpe_instance %}
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        {% endif %}
        z = x{{ type1.accessor }} {{ operator.operator }} y{{ type2.accessor }}
    END FUNCTION {{ operator.name }}_{{ type1.name }}_{{ type2.name }}
