    ELEMENTAL FUNCTION {{ operator.name }}_{{ type1.name }}_{{ type2.name }} (x, y) RESULT (z)
        {{ type1.declaration }}, INTENT(IN) :: x
        {{ type2.declaration }}, INTENT(IN) :: y
        {{ operator.return_type.declaration }} :: z
        z = x{{ type1.accessor }} {{ operator.operator }} y{{ type2.accessor }}
        {% if operator.reduce_precision %}
        CALL reduce_precision (z)
        {% endif %}
    END FUNCTION {{ operator.name }}_{{ type1.name }}_{{ type2.name }}
