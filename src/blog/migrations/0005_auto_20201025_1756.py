# Generated by Django 3.1.2 on 2020-10-25 17:56

from django.db import migrations


class Migration(migrations.Migration):

    dependencies = [
        ('blog', '0004_post_title'),
    ]

    operations = [
        migrations.RenameField(
            model_name='post',
            old_name='post',
            new_name='text',
        ),
    ]
