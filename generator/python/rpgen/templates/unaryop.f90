    ELEMENTAL FUNCTION {{ operator.name }}_{{ type1.name }} (x) RESULT (z)
        {{ type1.declaration }}, INTENT(IN) :: x
        {{ operator.return_type.declaration }} :: z
        z = {{ operator.operator }}(x{{ type1.accessor }})
    END FUNCTION {{ operator.name }}_{{ type1.name }}
