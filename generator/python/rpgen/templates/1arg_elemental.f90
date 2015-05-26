    ELEMENTAL FUNCTION {{ function.name }}_{{ type1.name }} (a) RESULT (x)
        {{ type1.declaration }}, INTENT(IN) :: a
        {{ function.return_type.declaration }} :: x
        x = {{ function.name.upper() }}(a{{ type1.accessor }})
        {% if function.reduce_precision %}
        CALL reduce_precision (x)
        {% endif %}
    END FUNCTION {{ function.name }}_{{ type1.name }}
