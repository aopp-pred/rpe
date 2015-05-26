    INTERFACE {{ interface_name }}
        {% for name in procedure_names %}
        MODULE PROCEDURE {{ name }}
        {% endfor %}
    END INTERFACE {{ interface_name }}
