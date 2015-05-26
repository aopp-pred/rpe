    ELEMENTAL FUNCTION {{ function.name }}_{{ type1.name }}_{{ type2.name }} (a, b) RESULT (x)
        {{ type1.declaration }}, INTENT(IN) :: a
        {{ type2.declaration }}, INTENT(IN) :: b
        {{ function.return_type.declaration }} :: x
        x = {{ function.name.upper() }}(a{{ type1.accessor }}, b{{ type2.accessor }})
        {% if function.reduce_precision %}
        CALL reduce_precision (x)
        {% endif %}
    END FUNCTION {{ function.name }}_{{ type1.name }}_{{ type2.name }}
